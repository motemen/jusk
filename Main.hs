{-
    Main.hs
    - http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/
    - http://lxr.mozilla.org/mozilla/source/js/src/
-}

module Main where
import IO hiding (try)
import List
import System.Environment hiding (getEnv)
import System.Console.GetOpt
import Control.Monad.State
import Control.Monad.Cont

import Parser
import ParserUtil
import DataTypes 
import Context
import Eval
import Repl
import Init

run :: [Flag] -> Evaluate a -> IO ()
run flags thunk =
    do nullEnv <- nullEnv flags
       (withCC CExit $ setupEnv >> thunk >> return Void) `runContT` (const $ return Void) `evalStateT` nullEnv
       return ()

try :: Evaluate a -> Evaluate ()
try thunk =
    do e <- withCC CThrow $ do { thunk; return Void }
       unless (isVoid e)
              (toString e >>= liftIO . ePutStrLn)

runRepl :: [Flag] -> IO ()
runRepl flags =
    run flags runReplWithTry

evalText :: String -> Evaluate Value
evalText input =
    case parse input of
         Left err -> liftIO $ printParseError input err
         Right program -> evalProgram program

evalFile :: [Flag] -> String -> IO ()
evalFile flags filename =
    do content <- readFile filename
       run flags $ try $ do
           evalText content
           when (EnterRepl `elem` flags)
                (runReplWithTry)

options :: [OptDescr Flag]
options = [
        Option ['d'] ["debug"]   (NoArg Debug)       "debug mode",
        Option ['w'] ["warn"]    (NoArg Warn)        "turn on warnings",
        Option ['p'] ["parse"]   (NoArg ParseOnly)   "only parse text (do not evaluate)",
        Option ['e'] []          (ReqArg EvalStr "") "evaluate string",
        Option ['r'] ["repl"]    (NoArg EnterRepl)   "enter repl (after reading file)",
        Option ['V'] ["version"] (NoArg Version)     "show version"
    ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
    case getOpt Permute options argv of
         (o, n, [])   -> return (o, n)
         (_, _, errs) -> do progName <- getProgName
                            ioError $ userError $ concat errs ++ usageInfo ("Usage: " ++ progName ++ " [OPTION...] [file]") options

printVersion :: IO ()
printVersion =
    putStrLn $ unwords $ words "jusk $Id$" \\ ["$Id:", "Main.hs", "motemen", "$"]

main :: IO ()
main =
    do args <- getArgs
       (flags, rest) <- parseOpts args
       when (Version `elem` flags) printVersion
       maybe (case length rest of
                   0 -> runRepl flags
                   _ -> evalFile flags $ rest !! 0)
             (\(EvalStr string) -> run flags $ try $ evalText string)
             (find isEvalStr flags)
    where isEvalStr (EvalStr _) = True
          isEvalStr _ = False
