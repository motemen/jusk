{-
    Repl.hs
-}

module Repl where
import Control.Monad.Cont
import Control.Monad.State
import IO

import Parser
import ParserUtil
import Context
import Eval
import DataTypes

runReplWithTry :: Evaluate ()
runReplWithTry =
    do e <- callCC $ \cc -> do
                pushCont cc CThrow
                return Void
       case e of
            Void -> runRepl'
            e  -> do liftIO $ ePutStrLn "uncaught exception:"
                     liftIO . ePutStrLn =<< toString e
                     runReplWithTry

runRepl' :: Evaluate ()
runRepl' =
    do line <- liftIO (putStr "js> " >> hFlush stdout >> getLine)
       value <- evalWithMoreInput line
       unless (isVoid value || isUndefined value)
              (do string <- toString value
                  liftIO $ putStrLn string
                  env <- getEnv
                  when (Debug `elem` envFlags env)
                       (liftIO $ putStrLn $ "-> " ++ inspect value))
       runRepl'
       where evalWithMoreInput input =
                 case parse input of
                      Left err | isErrorAtEnd input err
                                 -> do line <- liftIO (putStr "**> " >> hFlush stdout >> getLine)
                                       evalWithMoreInput $ input ++ "\n" ++ line
                      Left err | otherwise -> liftIO $ printParseError input err
                      Right program -> evalProgram program
