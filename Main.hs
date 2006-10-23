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
import PrettyShow

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePrint :: (Show a) => a -> IO ()
ePrint = hPrint stderr

run :: [Flag] -> Evaluate a -> IO ()
run flags thunk =
    do nullEnv <- nullEnv flags
       (withCC CExit $ setupEnv >> thunk >> return Void) `runContT` (const $ return Void) `evalStateT` nullEnv
       return ()

parse :: String -> Either ParseError JavaScriptProgram
parse = runLex program

printParseError :: String -> ParseError -> IO Value
printParseError input err =
    do ePutStrLn "parse error:"
       ePutStrLn $ showError input err
       return Void

evalProgram :: JavaScriptProgram -> Evaluate Value
evalProgram program =
    do env <- getEnv
       liftIO $ when (Debug `elem` (envFlags env)) (mapM_ ePrint program)
       liftIO $ when (Debug `elem` (envFlags env)) (mapM_ (ePutStrLn . prettyShow) program)
       if null program || ParseOnly `elem` (envFlags env)
          then return Void
          else liftM last $ mapM eval program

evalText :: String -> Evaluate Value
evalText input =
    case parse input of
         Left err -> liftIO $ printParseError input err
         Right program -> evalProgram program

evalFile :: [Flag] -> String -> IO ()
evalFile flags filename =
    do content <- readFile filename
       run flags $ do e <- callCC $ \cc -> do
                               pushCont cc CThrow
                               evalText content
                               return Void
                      when (isException e)
                           (liftIO $ print $ exceptionBody e)

runRepl' :: Evaluate ()
runRepl' =
    do line <- liftIO (putStr "js> " >> hFlush stdout >> getLine)
       value <- evalWithMoreInput line
       unless (isVoid value || isUndefined value)
              (do string <- toString value
                  liftIO $ putStrLn string)
       runRepl'
       where evalWithMoreInput input =
                 do case parse input of
                         Left err | isErrorAtEnd input err
                                    -> do line <- liftIO (putStr "**> " >> hFlush stdout >> getLine)
                                          evalWithMoreInput $ input ++ "\n" ++ line
                         Left err | otherwise -> liftIO $ printParseError input err
                         Right program -> evalProgram program

runRepl :: [Flag] -> IO ()
runRepl flags =
    run flags setupCatchAndRunRepl
    where setupCatchAndRunRepl =
              do e <- callCC $ \cc -> do
                          pushCont cc CThrow
                          return Void
                 case e of
                      Void -> runRepl'
                      Exception SysExit -> return ()
                      Exception e -> do liftIO $ print e
                                        setupCatchAndRunRepl
                      _ -> return () 

options :: [OptDescr Flag]
options = [
        Option ['d'] ["debug"]   (NoArg Debug)       "debug mode",
        Option ['w'] ["warn"]    (NoArg Warn)        "turn on warnings",
        Option ['p'] ["parse"]   (NoArg ParseOnly)   "only parse text (do not evaluate)",
        Option ['e'] []          (ReqArg EvalStr "") "evaluate string",
        Option ['V'] ["version"] (NoArg Version)     "show version"
    ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
    case getOpt Permute options argv of
         (o, n, [])   -> return (o, n)
         (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options
    where header = "Usage: jusk [OPTION...] [file]"

printVersion :: IO ()
printVersion =
    putStrLn "jusk $Id$ $Date$"

main :: IO ()
main =
    do args <- getArgs
       (flags, rest) <- parseOpts args
       when (Version `elem` flags) printVersion
       maybe (case length rest of
                   0 -> runRepl flags
                   _ -> evalFile flags $ rest !! 0)
             (\(EvalStr string) -> run flags $ evalText string)
             (find isEvalStr flags)
    where isEvalStr (EvalStr _) = True
          isEvalStr _ = False
