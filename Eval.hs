{-# OPTIONS_GHC -fglasgow-exts #-}
{-
    Eval.hs
    値の評価
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/10_Execution_Contexts.html
-}

module Eval (module Eval, Context.nullEnv, module JSType) where
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
        do c <- toBoolean =<< getValue =<< eval condition
           if c
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
               (evalDoWhileBlock Void)
        where evalDoWhileBlock lastValue =
                  do value <- withCC (CContinue Nothing) (eval block)
                     cond <- toBoolean =<< eval condition
                     if cond
                        then evalDoWhileBlock value
                        else return lastValue

    eval (STWhile condition block) =
        withCC (CBreak Nothing)
               (evalWhileBlock Void)
        where evalWhileBlock lastValue =
                  do cond <- toBoolean =<< eval condition
                     if cond
                        then do value <- withCC (CContinue Nothing) (eval block)
                                evalWhileBlock value
                        else return lastValue
              
    eval (STFor initialize condition update block) =
        withCC (CBreak Nothing)
               (do pushNullScope
                   eval initialize
                   value <- evalForBlock Void
                   popScope
                   return value)
        where evalForBlock lastValue =
                  do value <- toBoolean =<< eval condition
                     if value
                        then do value <- withCC (CContinue Nothing) (eval block)
                                eval update
                                evalForBlock value
                        else return lastValue

    eval (STForIn (STVariableDefinition { varDefBindings = [(name, _)] }) object block) =
        withCC (CBreak Nothing) 
               (do object <- readRef =<< evalR object
                   props <- liftAll $ return $ Map.keys $ objPropMap object
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

              evalSwitchStatement value clauses@((Just e, _):cs) defaultClause lastValue =
                  do e <- getValue =<< eval e
                     m <- toBoolean =<< comparisonOp (==) value e 
                     if m
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
           if bound
              then setVar name value
              else do warn $ "assignment to undeclared variable " ++ name
                      v <- defineVar name value
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
    maybe (throw $ NotImplemented $ "operator " ++ op)
          (\op -> do value <- (opUnaryFunc op) x
                     return $ tidyNumber value)
          (find (\p -> case p of
                            Unary op' _ -> op' == op
                            _ -> False)
                operatorsTable)

evalOperator op [x,y] =
    maybe (throw $ NotImplemented $ "operator " ++ op)
          (\op -> do value <- (opBinaryFunc op) x y
                     return $ tidyNumber value)
          (find (\p -> case p of
                            Binary op' _ -> op' == op
                            _ -> False) operatorsTable)

evalOperator op [x,y,z] =
    maybe (throw $ NotImplemented $ "operator " ++ op)
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

callFunction this (Object { objValue = obj }) args =
    callFunction this obj args

callFunction t o a =
    throw $ NotImplemented
          $ "callFunction:\n  this=" ++ show t ++ "\n  func=" ++ show o ++ "\n  args=" ++ show a

callMethod :: Value -> String -> [Value] -> Evaluate Value
callMethod object name args =
    do method <- getProp object name
       callFunction object method args

-- new Foo(arg1, arg2, ...)
-- TODO
new :: Value -> [Value] -> Evaluate Value
new (Object { objConstruct = NativeFunction constructor }) args = 
    constructor args

new (constructor@Function { }) args = 
    do prototype <- getProp constructor "prototype"
       object <- liftAll $ liftM Ref $ newIORef $ nullObject { objPrototype = prototype }
       callFunction object constructor args
       return object

new (ref@Ref { }) args =
    readRef ref >>= flip new args

defaultValue :: Value -> String -> Evaluate Value
defaultValue object hint =
    (if hint == "String" then liftM2 mplus (tryMethod "toString") (tryMethod "valueOf")
                         else liftM2 mplus (tryMethod "valueOf") (tryMethod "toString"))
     >>= maybe (throw $ NotImplemented $ "defaultValue: " ++ show object ++ " " ++ hint)
               (return)
    where tryMethod :: String -> Evaluate (Maybe Value)
          tryMethod name = 
              do method <- getProp object name
                 case method of
                      Null -> return Nothing
                      _ -> do result <- callFunction object method []
                              if isPrimitive result
                                 then return $ Just result
                                 else return Nothing
