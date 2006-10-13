{-# OPTIONS_GHC -fwarn-overlapping-patterns -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches #-}
{-
    Main.hs
    - http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/
    - http://lxr.mozilla.org/mozilla/source/js/src/
-}

module Main where
import System.Environment
import System.Console.GetOpt
import IO
import Control.Monad.State
import Control.Monad.Cont

import Parser
import DataTypes 
import Context
import Eval
import Init

data Flag
    = Debug
    | ParseOnly
    | InputFile String
    deriving (Show, Eq)

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePrint :: (Show a) => a -> IO ()
ePrint = hPrint stderr

run :: Evaluate a -> IO ()
run x = do nullFrame <- nullFrame
           x `runContT` (const $ return Void) `evalStateT` nullFrame
           return ()

evalText :: [Flag] -> String -> Evaluate Value
evalText flags input =
    case runLex program input of
         Left err ->
            liftAll
            $ do ePutStrLn "parse error:"
                 ePutStrLn $ showError input err
                 return Void
         Right program ->
             do liftAll $ when (Debug `elem` flags) (mapM_ ePrint program)
                if null program || ParseOnly `elem` flags
                   then return Void
                   else liftM last $ mapM eval program

evalFile :: [Flag] -> String -> IO ()
evalFile flags filename =
    do content <- readFile filename
       run $ do setupEnv
                e <- callCC $ \cc -> do { pushCont cc CThrow; evalText flags content; return Void }
                when (isException e)
                     (liftAll $ print $ exceptionBody e)

runRepl' :: [Flag] -> Evaluate ()
runRepl' flags =
    do line <- liftAll (putStr "js> " >> hFlush stdout >> getLine)
       value <- evalText flags line
       unless (isVoid value)
              (do string <- toString value
                  liftAll $ putStrLn string)
       runRepl' flags

runRepl :: [Flag] -> IO ()
runRepl flags =
    run (setupEnv >> setupCatchAndRunRepl)
    where setupCatchAndRunRepl =
              do e <- callCC $ \cc -> do { pushCont cc CThrow; return Void }
                 unless (isVoid e)
                        (do when (isException e) (liftAll $ print $ exceptionBody e)
                            setupCatchAndRunRepl)
                 runRepl' flags

options :: [OptDescr Flag]
options = [
        Option ['d'] ["debug"] (NoArg Debug)         "debug mode",
        Option ['p'] ["parse"] (NoArg ParseOnly)     "parse-only mode",
        Option []    []        (ReqArg InputFile "") "input file"
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
       case length rest of
            0 -> runRepl flags
            1 -> evalFile flags $ rest !! 0
