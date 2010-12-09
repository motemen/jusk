{-# OPTIONS_GHC -fglasgow-exts #-}
{-
    Eval.hs
    値の評価
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/10_Execution_Contexts.html
-}

module Eval (module Eval, module JSType) where
import IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Cont hiding (Cont)

import DataTypes
import {-# SOURCE #-} Operator
import {-# SOURCE #-} JSType
import {-# SOURCE #-} Internal
import Context

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

evalProgram :: JavaScriptProgram -> Evaluate Value
evalProgram program =
    do env <- getEnv
       if null program || ParseOnly `elem` envFlags env
          then return Void
          else if (Debug `elem` envFlags env)
                  then liftM last $ mapM (\e -> do { liftIO $ ePutStrLn $ "parsed: " ++ inspect e; eval e }) program
                  else liftM last $ mapM eval program

instance Eval Statement where
    eval (STVarDef bindings) =
        do mapM bindVariable bindings
           return Void
        where bindVariable (name, Nothing) =
                  defineVar name Undefined
              bindVariable (name, Just expr) =
                  eval expr >>= getValue >>= defineVar name

    eval (STFuncDef { funcDefName = name, funcDefFunc = func }) =
        do frames <- liftM envFrames getEnv
           protoObj <- makeNewObject
           defineVar name $ nullObject {
                   objPropMap = mkPropMap [("prototype", protoObj, [])],
                   objObject = func { funcScope = frames }
               }

    eval STEmpty =
        return Void
    
    eval (STExpression expr) =
        getValue =<< eval expr
    
    eval (STIf condition thenStatement maybeElseStatement) =
        ifM (toBoolean =<< getValue =<< eval condition)
            (eval thenStatement)
            (maybe (return Void) eval maybeElseStatement)
    
    eval (STBlock []) =
        return Void
    
    eval (STBlock statements) =
        liftM last $ mapM eval statements
    
    eval (STDoWhile condition block) =
        withCC (CBreak Nothing)
               (evalDoWhileBlock Void)
        where evalDoWhileBlock lastValue =
                  do value <- withCC (CContinue Nothing) (eval block)
                     ifM (toBoolean =<< eval condition)
                         (evalDoWhileBlock value)
                         (return lastValue)

    eval (STWhile condition block) =
        withCC (CBreak Nothing)
               (evalWhileBlock Void)
        where evalWhileBlock lastValue =
                  ifM (toBoolean =<< eval condition)
                      (do value <- withCC (CContinue Nothing) (eval block)
                          evalWhileBlock value)
                      (return lastValue)
              
    eval (STFor initialize condition update block) =
        withCC (CBreak Nothing)
               (do eval initialize
                   value <- evalForBlock Void
                   return value)
        where evalForBlock lastValue =
                  ifM (toBoolean =<< eval condition)
                      (do value <- withCC (CContinue Nothing) (eval block)
                          eval update
                          evalForBlock value)
                      (return lastValue)

    eval (STForIn init object block) =
        do name <- case init of
                        STVarDef [(name, expr)] ->
                            do defineVar name =<< maybe (return Undefined) eval expr
                               return name
                        STExpression (Identifier name) ->
                            do defineVar name Undefined        -- 既に定義されていてもどのみち上書きする
                               return name
                        _ -> throw "ReferenceError" "invalid left-hand side of for-in loop" >> return ""
           withCC (CBreak Nothing) 
                  (do object <- readRef =<< getValue =<< eval object
                      evalForInForObj name object)
        where evalForInForObj varName Object { objPropMap = props, objPrototype = proto } =
                  do forM (propToEnum props)
                          (evalForInBlock varName)
                     readRef proto >>= evalForInForObj varName
              evalForInForObj _ _ =
                  return Void
              evalForInBlock varName n =
                  do setVar varName $ String n
                     withCC (CContinue Nothing) (eval block)
              propToEnum = Map.keys . Map.filter (notElem DontEnum . propAttr)

    eval (STContinue label) =
        returnCont (CContinue label) Void

    eval (STBreak label) =
        returnCont (CBreak label) Void

    eval (STReturn (Just (Operator "()" (callee:args)))) =
        do callee <- eval callee
           mapM evalValue args >>= jumpToFunc callee

    eval (STReturn expr) =
        do value <- getValue =<< maybe (return Undefined) eval expr
           returnCont CReturn value

    eval (STWith expr st) =
        do object <- getValue =<< eval expr
           pushWithFrame object
           value <- eval st
           popFrame
           return value

    eval (STLabelled label st) =
        do callCC
           $ \cc -> do pushCont cc (CBreak $ Just label)
                       pushCont cc (CContinue $ Just label)
                       eval st

    eval (STSwitch expr statements) =
        do value <- getValue =<< eval expr
           withCC (CBreak Nothing) (evalSwitchStatement value statements Nothing Void)
        where evalSwitchStatement :: Value -> [(Maybe Expression, Statement)] -> Maybe Statement -> Value -> Evaluate Value
              evalSwitchStatement _ [] (Just st) _ =
                  eval st
                  
              evalSwitchStatement _ [] Nothing lastValue =
                  return lastValue

              evalSwitchStatement value ((Nothing, st):cs) Nothing lastValue =
                  evalSwitchStatement value cs (Just st) lastValue

              evalSwitchStatement value clauses@((Just e, _):cs) defaultClause lastValue =
                  do e <- getValue =<< eval e
                     ifM (toBoolean =<< comparisonOp (==) value e)
                         (liftM last $ mapM ((getValue =<<) . eval . snd) clauses)
                         (evalSwitchStatement value cs defaultClause lastValue)

    eval (STThrow expr) =
        do value <- getValue =<< eval expr
           returnCont CThrow value

    eval (STTry tryStatement catchClause finallyClause) =
        do e <- withCC CThrow (eval tryStatement >> return Void)
           v1 <- case catchClause of
                      Just (p, st) | not (isVoid e)
                           -> do binding <- bindParamArgs [p] [e]
                                 pushScope binding
                                 value <- eval st
                                 popScope
                                 return value
                      _ -> return Void
           v2 <- case finallyClause of
                      Just st -> eval st
                      Nothing -> return Void
           return $ v1 ||| v2
        where x ||| Void = x
              Void ||| y = y

evalValue :: (Eval a) => a -> Evaluate Value
evalValue x = getValue =<< eval x

-- Reference (Ref baseRef, p) の形になるまで評価する
instance Eval Expression where
    eval expr =
        do unless (isLiteral expr)
                  (debug $ "eval: " ++ show expr)
           eval' expr
        where
            eval' (Identifier name) =
                do frame <- liftM2 fromMaybe (liftM frObject currentFrame) (getVarFrameObject name)
                   return $ Reference frame name
            
            eval' (Keyword "this") =
                getThis
            
            eval' (Operator "[]" [base, p]) =
                do objRef <- getValue =<< eval base
                   prop <- toString =<< getValue =<< eval p
                   return $ Reference objRef prop
            
            eval' (Operator "()" (callee:args)) =
                do callee <- eval callee
                   mapM evalValue args >>= call callee
            
            eval' (Operator "new" (klass:args)) =
                do klass <- getValue =<< eval klass
                   args <- mapM evalValue args
                   construct klass args
            
            eval' (Operator "++" [x]) =
                eval $ Let x (Operator "+" [x, Literal $ Number $ Integer 1])

            eval' (Operator "_++" [x]) =
                do value <- readRef =<< evalValue x
                   eval $ Operator "++" [x]
                   return value

            eval' (Operator "--" [x]) =
                eval $ Let x (Operator "-" [x, Literal $ Number $ Integer 1])

            eval' (Operator "_--" [x]) =
                do value <- readRef =<< evalValue x
                   eval $ Operator "--" [x]
                   return value

            eval' (Operator "&&" [a, b]) =
                ifM (toBoolean =<< eval a)
                    (eval b)
                    (eval a)

            eval' (Operator "||" [a, b]) =
                ifM (toBoolean =<< eval a)
                    (eval a)
                    (eval b)

            eval' (Operator "?:" [c, t, e]) =
                ifM (toBoolean =<< eval c)
                    (eval t)
                    (eval e)

            eval' (Operator op exprs) =
                if elem op ["*=", "/=", "%=", "+=", "-=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]
                   then eval $ Let (head exprs) (Operator (init op) (tail exprs))
                   else do args <- mapM eval exprs
                           evalOperator op args
            
            eval' (ArrayLiteral exprs) =
                do items <- mapM evalValue exprs
                   makeNewArray items
            
            eval' (ObjectLiteral pairs) =
                do object <- makeNewObject
                   forM pairs $ \(n,e) -> do
                        n <- toString =<< eval n
                        p <- getValue =<< eval e
                        object ! n <~ p
                   return object

            eval' (RegExpLiteral pattern flags) =
                new "RegExp" [String pattern, String flags]
            
            eval' (List []) =
                return Void
            
            eval' (List exprs) =
                liftM last $ mapM evalValue exprs
            
            eval' (Let left right) =
                do left <- eval left
                   value <- getValue =<< eval right
                   putValue left value
                   return value
            
            eval' (Literal num@(Number _)) =
                return $ tidyNumber num
            
            eval' (Literal obj@Object { objObject = func@Function { } }) =
                do frames <- liftM envFrames getEnv
                   protoObj <- makeNewObject
                   return $ obj {
                           objPropMap = mkPropMap [("prototype", protoObj, [])],
                           objObject = func { funcScope = frames }
                        }

            eval' (Literal value) =
                return value
            
            eval' expr =
                return $ String $ show expr
-- }}}

-- Operator {{{
evalOperator :: String -> [Value] -> Evaluate Value
evalOperator op [x] =
    maybe (throw "NotImplemented" $ "operator " ++ op)
          (\opDef -> liftM tidyNumber $ opUnaryFunc' opDef x)
          (find (isUnaryOp op) operatorsTable)
    where isUnaryOp op (Unary op' _) = op == op'
          isUnaryOp _ _ = False

evalOperator op [x,y] =
    maybe (throw "NotImplemented" $ "operator " ++ op)
          (\opDef -> liftM tidyNumber $ opBinaryFunc' opDef x y)
          (find (isBinaryOp op) operatorsTable)
    where isBinaryOp op (Binary op' _) = op == op'
          isBinaryOp _ _ = False

evalOperator _ _ =
    return Undefined
-- }}}

-- [[Call]]
call :: Value -> [Value] -> Evaluate Value
call Reference { refBase = base, refName = name } args =
    callMethod base name args

call function args =
    do global <- getGlobal
       debug $ "call: " ++ show function ++ " " ++ show args
       callWithThis global function args

callWithThis :: Value -> Value -> [Value] -> Evaluate Value
callWithThis this callee@Object { objName = name,
                                  objObject = Function {
                                      funcParam = param,
                                      funcBody = body,
                                      funcScope = scope
                                  }
                                } args =
    do debug $ "callWithThis: " ++ show this ++ " " ++ name ++ " " ++ show args
       arguments <- makeArguments
       binding <- makeRef =<< bindParamArgs (["arguments"] ++ param) ([arguments] ++ args ++ repeat Undefined)
       withScope scope
                 $ do pushFrame this binding
                      withCC CReturn (eval body >> returnCont CReturn Undefined)
    where makeArguments
              = return $ nullObject { objPropMap = mkPropMap argProps }
          argProps = zip3 (map show [0..]) (args) (repeat [DontEnum]) ++
                         [("callee", callee, [DontEnum]), ("length", toValue $ length args, [DontEnum])]

callWithThis this Object { objName = name, objObject = NativeFunction { funcNatCode = nativeFunc } } args =
    do debug $ "callWithThis: " ++ name
       nativeFunc this args

callWithThis _ Object { objName = name } _ =
    throw "TypeError" $ name ++ " is not a function"

callWithThis this ref@Ref { } args =
    do object <- readRef ref
       callWithThis this object args

callWithThis this ref@Reference { } args =
    do callee <- getValue ref
       ifM (toBoolean callee)
           (callWithThis this callee args)
           (throw "TypeError" $ show callee ++ " is not a function")

callWithThis _ value _ =
    throw "TypeError" $ show value ++ " is not a function"

callMethod :: Value -> String -> [Value] -> Evaluate Value
callMethod object name args =
    do debug $ "callMethod: " ++ show object ++ " " ++ name ++ " " ++ show args
       method <- readRef =<< getProp object name
       if isFunction method || isNativeFunction method
          then callWithThis object method args
          else throw "TypeError" $ getName object ++ "." ++ name ++ " is not a function"

-- 末尾再帰用
jumpToFunc :: Value -> [Value] -> Evaluate Value
jumpToFunc Reference { refBase = base, refName = name } args =
    jumpToMethod base name args

jumpToFunc function args =
    do global <- getGlobal
       jumpToFuncWithThis global function args

jumpToFuncWithThis :: Value -> Value -> [Value] -> Evaluate Value
jumpToFuncWithThis this callee@Object { objObject = Function { funcParam = param, funcBody = body, funcScope = scope } } args =
    do debug $ "jumpToFuncWithThis: " ++ objName callee
       arguments <- makeArguments
       binding <- makeRef =<< bindParamArgs (["arguments"] ++ param) ([arguments] ++ args ++ repeat Undefined)
       modifyScope scope
       pushFrame this binding
       eval body
    where makeArguments
              = return $ nullObject { objPropMap = mkPropMap argProps }
          argProps = zip3 (map show [0..]) (args) (repeat [DontEnum]) ++
                         [("callee", callee, [DontEnum]), ("length", toValue $ length args, [DontEnum])]

jumpToFuncWithThis this Object { objName = name, objObject = NativeFunction { funcNatCode = nativeFunc } } args =
    do debug $ "jumpToFuncWithThis: " ++ name
       returnCont CReturn =<< nativeFunc this args

jumpToFuncWithThis this ref@Ref { } args =
    do object <- readRef ref
       jumpToFuncWithThis this object args

jumpToFuncWithThis _ object _ =
    throw "TypeError" $ getName object ++ " is not a function"

jumpToMethod :: Value -> String -> [Value] -> Evaluate Value
jumpToMethod object name args =
    do method <- readRef =<< getProp object name
       if isFunction method || isNativeFunction method
          then jumpToFuncWithThis object method args
          else throw "TypeError" $ getName object ++ "." ++ name ++ " is not a function"

-- [[Construct]]
construct :: Value -> [Value] -> Evaluate Value
construct obj@Object { objConstruct = Just constructor } args =
    do proto <- getProp obj "prototype"
       object <- makeRef =<< constructor Null args
       modifyValue object $ setObjProto proto
       return object

construct obj@Object { objObject = Function { } } args = 
    do proto <- getProp obj "prototype"
       object <- makeRef $ nullObject { objPrototype = proto }
       callWithThis object obj args
       return object

construct obj@Object { objObject = NativeFunction { } } args =
    do proto <- getProp obj "prototype"
       object <- makeRef $ nullObject { objPrototype = proto }
       callWithThis object obj args
       return object

construct ref@Ref { } args =
    readRef ref >>= flip construct args

construct Object { objValue = value } args | not $ isNull value =
    construct value args

construct c _ =
    throw "TypeError" $ getName c ++ " is not a constructor"

new :: String -> [Value] -> Evaluate Value
new name args =
    do klass <- getVar name
       makeRef =<< construct klass args

defaultValue :: Value -> String -> Evaluate Value
defaultValue object hint =
    (case hint of
          "String" -> liftM2 mplus (tryMethod "toString") (tryMethod "valueOf")
          "Number" -> liftM2 mplus (tryMethod "valueOf")  (tryMethod "toString")
          _ -> ifM (liftM ("Date" ==) $ classOf object)
                   (liftM2 mplus (tryMethod "toString") (tryMethod "valueOf"))
                   (liftM2 mplus (tryMethod "valueOf")  (tryMethod "toString")))
    >>= maybe (throw "NotImplemented" $ "defaultValue: " ++ show object ++ " " ++ hint)
              (return)
    where tryMethod :: String -> Evaluate (Maybe Value)
          tryMethod name =
              do method <- getProp object name
                 if isNull method || isUndefined method
                    then return Nothing
                    else do result <- callWithThis object method []
                            if isPrimitive result
                               then return $ Just result
                               else return Nothing

try :: Evaluate a -> Evaluate ()
try thunk =
    do e <- withCC CThrow $ do { thunk; return Void }
       unless (isVoid e)
              (toString e >>= liftIO . ePutStrLn)
