{-# OPTIONS_GHC -fglasgow-exts #-}
{-
    Eval.hs
    値の評価
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/10_Execution_Contexts.html
    http://www.mozilla.org/js/language/js20/core/namespaces.html#property-lookup
-}

module Eval (module Eval, Context.nullFrame) where
import Monad
import Data.IORef
import List
import Maybe
import Control.Monad.State
import Control.Monad.Cont hiding(Cont)

import Parser
import {-# SOURCE #-} Operator
import DataTypes
import Internal
import Context

tidyNumber :: Value -> Value
tidyNumber num@(Number (Double x))
    | isInfinite x || isNaN x = num 
    | x == (fromIntegral $ round x) = Number $ Integer $ round x
    | otherwise = num
tidyNumber x = x

instance Eval Statement where
    eval (STVariableDefinition bindings) =
        do mapM bindVariable bindings
           return Void
        where bindVariable (name, Nothing) =
                  defineVar name Undefined
              bindVariable (name, Just expr) =
                  eval expr >>= getValue >>= defineVar name

    eval (STFunctionDefinition { funcDefFunc = function@Function { funcName = Just name } }) =
        defineVar name function

    eval STEmpty =
        return Void
    
    eval (STExpression expr) =
        getValue =<< eval expr
    
    eval (STIf condition thenStatement maybeElseStatement) =
        do c <- getValue =<< eval condition
           if fromValue c
              then eval thenStatement
              else maybe (return Void)
                         (eval)
                         (maybeElseStatement)
    
    eval (STBlock []) =
        return Void
    
    eval (STBlock statements) =
        liftM last $ mapM eval statements
    
    eval (STDoWhile condition block) =
        withCC (CBreak Nothing)
               (evalDoWhileBlock condition block Void)
        where evalDoWhileBlock condition block lastValue =
                  do value <- withCC (CContinue Nothing) (eval block)
                     cond <- eval condition
                     if fromValue cond
                        then evalDoWhileBlock condition block value
                        else return lastValue

    eval (STWhile condition block) =
        withCC (CBreak Nothing)
               (evalWhileBlock condition block Void)
        where evalWhileBlock condition block lastValue =
                  do cond <- eval condition
                     if fromValue cond
                        then do value <- withCC (CContinue Nothing) (eval block)
                                evalWhileBlock condition block value
                        else return lastValue
              
    eval (STFor initialize condition update block) =
        withCC (CBreak Nothing)
               (do pushNullScope
                   eval initialize
                   value <- evalForBlock condition block update Void
                   popScope
                   return value)
        where evalForBlock condition block update lastValue =
                  do value <- eval condition
                     if fromValue value
                        then do value <- withCC (CContinue Nothing) (eval block)
                                eval update
                                evalForBlock condition block update value
                        else return lastValue

    eval (STForIn (STVariableDefinition { varDefBindings = [(name, _)] }) object block) =
        withCC (CBreak Nothing) 
               (do object <- readRef =<< evalR object
                   props <- liftAll $ return $ map fst $ properties object
                   liftM last $ mapM (\n -> do binding <- bindParamArgs [name] [String n]
                                               pushScope binding
                                               value <- eval block
                                               popScope
                                               return value)
                                     props)

    eval (STContinue label) =
        returnCont (CContinue label) Void

    eval (STBreak label) =
        returnCont (CBreak label) Void

    eval (STReturn expr) =
        do value <- maybe (return Undefined) eval expr
           returnCont CReturn value

    eval (STWith expr st) =
        do Ref objRef <- getValue =<< eval expr
           pushWithFrame objRef
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

              evalSwitchStatement value clauses@((Just e, st):cs) defaultClause lastValue =
                  do e <- getValue =<< eval e
                     m <- comparisonOp (==) value e 
                     if fromValue m
                        then liftM last $ mapM (evalR . snd) clauses
                        else evalSwitchStatement value cs defaultClause lastValue

    eval (STThrow expr) =
        do value <- eval expr
           returnCont CThrow value

    eval (STTry tryStatement catchClause finallyClause) =
        do e <- withCC CThrow (eval tryStatement)
           v1 <- case e of
                      Exception _ ->
                          if isNothing catchClause
                             then return Void
                             else do let (p, st) = fromJust catchClause
                                     binding <- bindParamArgs [p] [e]
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

evalR :: (Eval a) => a -> Evaluate Value
evalR x =
    eval x >>= getValue

-- Reference (Ref baseRef, p) の形になるまで評価する
instance Eval Expression where
    eval (Identifier name) =
        getVar name
    
    eval (Keyword "this") =
        getThis
    
    eval (Operator "[]" [Identifier name, p]) =
        do baseRef <- liftM getRef $ getVar name
           prop <- toString =<< getValue =<< eval p
           return $ Reference (baseRef, prop)
    
    eval (Operator "[]" [base, p]) =
        do Ref baseRef <- makeRef =<< getValue =<< eval base -- XXX: あやしい
           prop <- toString =<< getValue =<< eval p
           return $ Reference (baseRef, prop)
    
    eval (Operator "()" (callee:args)) =
        do maybeRef <- eval callee
           this <- liftAll $ return
                           $ case maybeRef of
                                  Reference (baseRef, _) -> Ref baseRef
                                  _ -> Null
           callee <- getValue =<< eval callee
--         liftAll $ do { putStr "eval \"()\": "; print (maybeRef, this, callee) }
           mapM evalR args >>= callFunction this callee 
    
    eval (Operator "new" (klass:args)) =
        do klass <- getValue =<< eval klass
           args <- mapM evalR args
           new klass args
    
    eval (Operator "++" [x]) =
        eval $ Let x (Operator "+" [x, Literal $ Number $ Integer 1])

    eval (Operator "_++" [x]) =
        do value <- eval x
           eval $ Operator "++" [x]
           return value

    eval (Operator "--" [x]) =
        eval $ Let x (Operator "-" [x, Literal $ Number $ Integer 1])

    eval (Operator "_--" [x]) =
        do value <- eval x
           eval $ Operator "--" [x]
           return value

    eval (Operator op exprs) =
        if elem op ["*=", "/=", "%=", "+=", "-=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]
              then eval $ Let (head exprs) (Operator (init op) (tail exprs))
              else do args <- mapM evalR exprs
                      args <- mapM readRef args
                      evalOperator op args
    
    eval (ArrayLiteral exprs) =
        do constructor <- getVar "Array"
           array <- new constructor []
           array <- liftAll $ liftM Ref $ newIORef array
           items <- mapM evalR exprs
           callMethod array "push" items
           return array
    
    eval (ObjectLiteral pairs) =
        do constructor <- getVar "Object"
           object <- new constructor []
           objRef <- liftAll $ newIORef object
           mapM ((\(n,e) -> do n <- toString =<< eval n
                               p <- getValue =<< eval e
                               putProp objRef n p)) pairs
           return $ Ref objRef
    
    eval (List []) =
        return Void
    
    eval (List exprs) =
        liftM last $ mapM evalR exprs
    
    eval (Let (Identifier name) right) =
        do bound <- isBound name
           value <- getValue =<< eval right
           -- TODO: warn if not bound
           if bound
              then setVar name value
              else do v <- defineVar name value
                      return v
    
    eval (Let left right) =
        do left <- eval left
           value <- getValue =<< eval right
           putValue left value
           return value
    
    eval (Literal num@(Number _))
        = return $ tidyNumber num
    
    eval (Literal value) =
        return value
    
    eval expr =
        return $ String $ show expr
-- }}}

-- Operator {{{
evalOperator :: String -> [Value] -> Evaluate Value
evalOperator op [x] =
    maybe (return Undefined) -- TODO: throw error
          (\op -> do value <- (opUnaryFunc op) x
                     return $ tidyNumber value)
          (find (\p -> case p of
                            Unary op' _ -> op' == op
                            _ -> False)
                operatorsTable)

evalOperator op [x,y] =
    maybe (return Undefined) -- TODO: throw error
          (\op -> do value <- (opBinaryFunc op) x y
                     return $ tidyNumber value)
          (find (\p -> case p of
                            Binary op' _ -> op' == op
                            _ -> False) operatorsTable)

evalOperator op [x,y,z] =
    maybe (return Undefined) -- TODO: throw error
          (\op -> do value <- (opTernaryFunc op) x y z
                     return $ tidyNumber value)
          (find (\p -> case p of
                            Ternary op' _ -> op' == op
                            _ -> False) operatorsTable)

evalOperator _ _ =
    return Undefined
-- }}}

callFunction :: Value -> Value -> [Value] -> Evaluate Value
callFunction this (Function { funcParam = param, funcBody = body }) args =
    do binding <- bindParamArgs param (args ++ repeat Undefined)
       pushFrame this binding
       value <- withCC CReturn (eval body)
       popFrame
       return value

callFunction this (NativeFunction nativeFunc) args =
    do pushNullFrame this
       value <- nativeFunc args
       popFrame
       return value

callFunction this (Ref objRef) args =
    do object <- liftAll $ readIORef objRef
       callFunction this object args

callFunction this (Object { delegate = obj }) args =
    callFunction this obj args

-- XXX: Debug
callFunction t o a =
    return $ String $ "callFunction:\n  this=" ++ show t ++ "\n  func=" ++ show o ++ "\n  args=" ++ show a

callMethod :: Value -> String -> [Value] -> Evaluate Value
callMethod object name args =
    do method <- getProp object name
       callFunction object method args

-- new Foo(arg1, arg2, ...)
-- TODO
new :: Value -> [Value] -> Evaluate Value
new (Object { construct = NativeFunction constructor }) args = 
    constructor args

new (constructor@Function { }) args = 
    do prototype <- getProp constructor "prototype"
       object <- liftAll $ liftM Ref $ newIORef $ nullObject { prototype = prototype }
       callFunction object constructor args
       return object

new (ref@Ref { }) args =
    readRef ref >>= flip new args

toString :: Value -> Evaluate String
toString Void      = return ""
toString Undefined = return "undefined"
toString Null      = return "null"

toString (Boolean False) = return "false"
toString (Boolean True)  = return "true"

toString (String string) = return string

toString (Number (Integer n)) = return $ show n
toString (Number (Double n))  = return $ show n
toString (Number NaN)         = return "NaN"

toString (NativeFunction _) =
    return "[native function]"

toString ref@(Reference _) =
    do object <- getValue ref
       toString object

toString (Ref objRef) =
    do object <- liftAll $ readIORef objRef
       toString object

toString (Exception e) =
    return $ show e

toString object = 
    do String s <- callMethod object "toString" []
       return s

toNumber :: Value -> Evaluate Number
toNumber Undefined =
    return NaN

toNumber Null =
    return $ Integer 0

toNumber (Boolean bool) =
    return $ if bool then Integer 1
                     else Double 0.0

toNumber (Number num) =
    return num

toNumber (String string) =
    case runLex numericLiteral string of
         Left _ -> return NaN
         Right (Literal (Number n)) -> return n

toNumber object@(Object { }) =
    toPrimitive object "Number" >>= toNumber

toPrimitive :: Value -> String -> Evaluate Value
toPrimitive Undefined _ =
    return Undefined

toPrimitive Null _ =
    return Null

toPrimitive bool@(Boolean _) _ =
    return bool

toPrimitive num@(Number _) _ =
    return num

toPrimitive string@(String _) _ =
    return string

toPrimitive object preferredType =
    defaultValue object preferredType

defaultValue :: Value -> String -> Evaluate Value
defaultValue object hint =
    (if hint == "String" then liftM2 mplus (tryMethod object "toString") (tryMethod object "valueOf")
                         else liftM2 mplus (tryMethod object "valueOf") (tryMethod object "toString"))
     >>= maybe (return Null) -- TODO: Warn
               (return)
    where tryMethod :: Value -> String -> Evaluate (Maybe Value)
          tryMethod object name = 
              do method <- getProp object name
                 case method of
                      Null -> return Nothing
                      _ -> do result <- callFunction object method []
                              if isPrimitive result
                                 then return $ Just result
                                 else return Nothing
