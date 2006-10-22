{-# OPTIONS_GHC -fglasgow-exts #-}
{-
    Eval.hs
    値の評価
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/10_Execution_Contexts.html
-}

module Eval (module Eval, module JSType) where
import qualified Data.Map as Map
import Data.IORef
import List
import Maybe
import Control.Monad.Cont hiding(Cont)

import DataTypes
import {-# SOURCE #-} Operator
import {-# SOURCE #-} JSType
import Internal
import Context

instance Eval Statement where
    eval (STVarDef bindings) =
        do mapM bindVariable bindings
           return Void
        where bindVariable (name, Nothing) =
                  defineVar name Undefined
              bindVariable (name, Just expr) =
                  eval expr >>= getValue >>= defineVar name

    eval (STFuncDef { funcDefFunc = func@Function { funcName = name } }) =
        do frames <- liftM envFrames getEnv
           defineVar name $ func { funcScope = frames }

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
        do var <- case init of
                       STVarDef [(name, expr)] ->
                           defineVar name =<< maybe (return Undefined) eval expr
                       STExpression (Identifier name) ->
                           defineVar name Undefined        -- 既に定義されていてもどのみち上書きする
                       STExpression (List _) ->
                           throw $ ReferenceError $ "invalid left-hand side of for-in loop"
                       STExpression expr ->
                           eval expr
                       _ -> throw $ ReferenceError $ "invalid left-hand side of for-in loop"
           withCC (CBreak Nothing) 
                  (do object <- readRef =<< getValue =<< eval object
                      liftM lastOrVoid $ mapM (evalForInBlock var)
                                       $ Map.keys $ Map.filter (notElem DontEnum . propAttr) $ objPropMap object)
        where evalForInBlock var n =
                  do putValue var $ String n
                     eval block
              lastOrVoid [] = Void
              lastOrVoid xs = last xs

    eval (STContinue label) =
        returnCont (CContinue label) Void

    eval (STBreak label) =
        returnCont (CBreak label) Void

    eval (STReturn (Just (Operator "()" (callee:args)))) =
        do maybeRef <- eval callee
           let this = case maybeRef of
                           Reference base _ -> base
                           _ -> Null
           callee <- getValue =<< eval callee
           mapM evalValue args >>= jumpToFunc this callee 

    eval (STReturn expr) =
        do value <- maybe (return Undefined) eval expr
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
        do value <- eval expr
           returnCont CThrow value

    eval (STTry tryStatement catchClause finallyClause) =
        do e <- withCC CThrow (eval tryStatement)
           v1 <- case catchClause of
                      Just (p, st) | isException e
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
    eval (Identifier name) =
        getVar name
    
    eval (Keyword "this") =
        getThis
    
    eval (Operator "[]" [Identifier name, p]) =
        do baseRef <- getVar name
           prop <- toString =<< getValue =<< eval p
           return $ Reference baseRef prop
    
    eval (Operator "[]" [base, p]) =
        do objRef <- getValue =<< eval base
           prop <- toString =<< getValue =<< eval p
           return $ Reference objRef prop
    
    eval (Operator "()" (callee:args)) =
        do maybeRef <- eval callee
           let this = case maybeRef of
                           Reference base _ -> base
                           _ -> Null
           callee <- getValue =<< eval callee
           mapM evalValue args >>= call this callee 
    
    eval (Operator "new" (klass:args)) =
        do klass <- getValue =<< eval klass
           args <- mapM evalValue args
           construct klass args
    
    eval (Operator "++" [x]) =
        eval $ Let x (Operator "+" [x, Literal $ Number $ Integer 1])

    eval (Operator "_++" [x]) =
        do value <- readRef =<< evalValue x
           eval $ Operator "++" [x]
           return value

    eval (Operator "--" [x]) =
        eval $ Let x (Operator "-" [x, Literal $ Number $ Integer 1])

    eval (Operator "_--" [x]) =
        do value <- readRef =<< evalValue x
           eval $ Operator "--" [x]
           return value

    eval (Operator "===" [x, y]) =
        do x <- evalValue x
           y <- evalValue y
           return $ toValue $ x == y

    eval (Operator "!==" [x, y]) =
        do x <- evalValue x
           y <- evalValue y
           return $ toValue $ x /= y

    eval (Operator op exprs) =
        if elem op ["*=", "/=", "%=", "+=", "-=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]
           then eval $ Let (head exprs) (Operator (init op) (tail exprs))
           else do args <- mapM readRef =<< mapM evalValue exprs
                   evalOperator op args
    
    eval (ArrayLiteral exprs) =
        do constructor <- getVar "Array"
           array <- makeRef =<< construct constructor []
           items <- mapM evalValue exprs
           unless (null items)
                  (callMethod array "push" items >> return ())
           return array
    
    eval (ObjectLiteral pairs) =
        do constructor <- getVar "Object"
           object <- makeRef =<< construct constructor []
           forM pairs $ \(n,e) -> do
                n <- toString =<< eval n
                p <- getValue =<< eval e
                object ! n <~ p
           return object
    
    eval (List []) =
        return Void
    
    eval (List exprs) =
        liftM last $ mapM evalValue exprs
    
    eval (Let (Identifier name) right) =
        do value <- getValue =<< eval right
           ifM (isBound name)
               (setVar name value)
               (do warn $ "assignment to undeclared variable " ++ name
                   defineVar name value)
    
    eval (Let left right) =
        do left <- eval left
           value <- getValue =<< eval right
           putValue left value
           return value
    
    eval (Literal num@(Number _)) =
        return $ tidyNumber num
    
    eval (Literal func@Function { }) =
        do frames <- liftM envFrames getEnv
           return $ func { funcScope = frames }

    eval (Literal value) =
        return value
    
    eval expr =
        return $ String $ show expr
-- }}}

-- Operator {{{
evalOperator :: String -> [Value] -> Evaluate Value
evalOperator op [x] =
    maybe (throw $ NotImplemented $ "operator " ++ op)
          (\op -> liftM tidyNumber $ opUnaryFunc op x)
          (find (isUnaryOp op) operatorsTable)
    where isUnaryOp op (Unary op' _) = op == op'
          isUnaryOp _ _ = False

evalOperator op [x,y] =
    maybe (throw $ NotImplemented $ "operator " ++ op)
          (\op -> liftM tidyNumber $ opBinaryFunc op x y)
          (find (isBinaryOp op) operatorsTable)
    where isBinaryOp op (Binary op' _) = op == op'
          isBinaryOp _ _ = False

evalOperator op [x,y,z] =
    maybe (throw $ NotImplemented $ "operator " ++ op)
          (\op -> liftM tidyNumber $ opTernaryFunc op x y z)
          (find (isTernaryOp op) operatorsTable)
    where isTernaryOp op (Ternary op' _) = op == op'
          isTernaryOp _ _ = False

evalOperator _ _ =
    return Undefined
-- }}}

-- [[Call]]
call :: Value -> Value -> [Value] -> Evaluate Value
call this (callee@Function { funcParam = param, funcBody = body, funcScope = scope }) args =
    do arguments <- makeArguments
       binding <- makeRef =<< bindParamArgs (["arguments"] ++ param) ([arguments] ++ args ++ repeat Undefined)
       withScope scope
                 $ do pushFrame this binding
                      withCC CReturn (eval body >> returnCont CReturn Undefined)
    where makeArguments
              = do proto <- prototypeOfVar "Object"
                   return $ nullObject { objPropMap = mkPropMap argProps, objPrototype = proto }
          argProps = zip3 (map show [0..]) (args) (repeat [DontEnum]) ++
                         [("callee", callee, [DontEnum]), ("length", toValue $ length args, [DontEnum])]

call this (NativeFunction { funcNatCode = nativeFunc }) args =
    do pushNullFrame this
       value <- nativeFunc args
       popFrame
       return value

call this ref@Ref { } args =
    do object <- readRef ref
       call this object args

call _ object _ =
    throw $ TypeError $ show object ++ " is not a function"

callMethod :: Value -> String -> [Value] -> Evaluate Value
callMethod object name args =
    do method <- readRef =<< getProp object name
       if isFunction method || isNativeFunction method
          then call object method args
          else do objString <- toString object
                  throw $ TypeError $ objString ++ "." ++ name ++ " is not a function"

-- 末尾再帰用
jumpToFunc :: Value -> Value -> [Value] -> Evaluate Value
jumpToFunc this (callee@Function { funcParam = param, funcBody = body, funcScope = scope }) args =
    do arguments <- makeArguments
       binding <- makeRef =<< bindParamArgs (["arguments"] ++ param) ([arguments] ++ args ++ repeat Undefined)
       modifyScope scope
       pushFrame this binding
       eval body
    where makeArguments
              = do proto <- prototypeOfVar "Object"
                   return $ nullObject { objPropMap = mkPropMap argProps, objPrototype = proto }
          argProps = zip3 (map show [0..]) (args) (repeat [DontEnum]) ++
                         [("callee", callee, [DontEnum]), ("length", toValue $ length args, [DontEnum])]

jumpToFunc this (NativeFunction { funcNatCode = nativeFunc }) args =
    do pushNullFrame this
       value <- nativeFunc args
       popFrame
       return value

jumpToFunc this ref@Ref { } args =
    do object <- readRef ref
       jumpToFunc this object args

jumpToFunc _ object _ =
    throw $ TypeError $ show object ++ " is not a function"

-- [[Construct]]
construct :: Value -> [Value] -> Evaluate Value
construct (func@Function { }) args = 
    do proto <- getProp func "prototype"
       object <- makeRef $ nullObject { objPrototype = proto }
       call object func args
       return object

construct func@NativeFunction { funcConstruct = Just constructor } args =
    do proto <- getProp func "prototype"
       object <- makeRef =<< constructor args
       liftIO $ modifyIORef (getRef object) $ setObjProto proto
       return object

construct func@NativeFunction { } args =
    do proto <- getProp func "prototype"
       object <- makeRef $ nullObject { objPrototype = proto }
       call object func args
       return object

construct ref@Ref { } args =
    readRef ref >>= flip construct args

construct Object { objValue = value } args | not $ isNull value =
    construct value args

construct c _ =
    throw $ TypeError $ show c ++ " is not a constructor"

defaultValue :: Value -> String -> Evaluate Value
defaultValue object hint =
    -- XXX: hintが提供されないとき: Date オブジェクトなら "String", それ以外は "Number"
    (case hint of
          "String" -> liftM2 mplus (tryMethod "toString") (tryMethod "valueOf")
          "Number" -> liftM2 mplus (tryMethod "valueOf")  (tryMethod "toString")
          _ -> do klass <- classOf object
                  if klass == "Date"
                     then liftM2 mplus (tryMethod "toString") (tryMethod "valueOf")
                     else liftM2 mplus (tryMethod "valueOf")  (tryMethod "toString"))
     >>= maybe (throw $ NotImplemented $ "defaultValue: " ++ show object ++ " " ++ hint)
               (return)
    where tryMethod :: String -> Evaluate (Maybe Value)
          tryMethod name = 
              do method <- getProp object name
                 case method of
                      Null -> return Nothing
                      Undefined -> return Nothing
                      _ -> do result <- call object method []
                              if isPrimitive result
                                 then return $ Just result
                                 else return Nothing

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mc mt me =
    do cond <- mc
       if cond
          then mt
          else me
