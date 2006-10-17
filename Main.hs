{-# OPTIONS_GHC -fwarn-overlapping-patterns -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches #-}
{-
    Main.hs
    - http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/
    - http://lxr.mozilla.org/mozilla/source/js/src/
-}

module Main where
import System.Environment hiding(getEnv)
import System.Console.GetOpt
import IO
import Control.Monad.State
import Control.Monad.Cont
import List

import Parser
import ParserUtil
import DataTypes 
import Context
import Eval
import Init

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePrint :: (Show a) => a -> IO ()
ePrint = hPrint stderr

run :: [Flag] -> Evaluate a -> IO ()
run flags thunk =
    do nullEnv <- nullEnv flags
       (setupEnv >> thunk) `runContT` (const $ return Void) `evalStateT` nullEnv
       return ()

evalText :: String -> Evaluate Value
evalText input =
    case runLex program input of
         Left err ->
            liftAll
            $ do ePutStrLn "parse error:"
                 ePutStrLn $ showError input err
                 return Void
         Right program ->
             do env <- getEnv
                liftAll $ when (Debug `elem` (envFlags env)) (mapM_ ePrint program)
                if null program || ParseOnly `elem` (envFlags env)
                   then return Void
                   else liftM last $ mapM eval program

evalFile :: [Flag] -> String -> IO ()
evalFile flags filename =
    do content <- readFile filename
       run flags $ do e <- callCC $ \cc -> do { pushCont cc CThrow; evalText content; return Void }
                      when (isException e)
                           (liftAll $ print $ exceptionBody e)

runRepl' :: Evaluate ()
runRepl' =
    do line <- liftAll (putStr "js> " >> hFlush stdout >> getLine)
       value <- evalText line
       unless (isVoid value)
              (do string <- toString value
                  liftAll $ putStrLn string)
       runRepl'

runRepl :: [Flag] -> IO ()
runRepl flags =
    run flags setupCatchAndRunRepl
    where setupCatchAndRunRepl =
              do e <- callCC $ \cc -> do { pushCont cc CThrow; return Void }
                 case e of
                      Void -> runRepl'
                      Exception SysExit -> return ()
                      Exception e -> do liftAll $ print e
                                        setupCatchAndRunRepl
                      _ -> return () 

options :: [OptDescr Flag]
options = [
        Option ['d'] ["debug"] (NoArg Debug)       "debug mode",
        Option ['w'] ["warn"]  (NoArg Warn)        "turn on warnings",
        Option ['p'] ["parse"] (NoArg ParseOnly)   "only parse text (do not evaluate)",
        Option ['e'] []        (ReqArg EvalStr "") "evaluate string"
    ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
    case getOpt Permute options argv of
         (o, n, [])   -> return (o, n)
         (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options
    where header = "Usage: jusk [OPTION...] [file]"

main :: IO ()
main =
    do args <- getArgs
       (flags, rest) <- parseOpts args
       maybe (case length rest of
                   0 -> runRepl flags
                   1 -> evalFile flags $ rest !! 0)
             (\(EvalStr string) -> run flags $ evalText string)
             (find isEvalStr flags)
    where isEvalStr (EvalStr _) = True
          isEvalStr _ = False
