import Brainfuck

import System.Console.Readline
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
import Data.List (group)
import Control.Monad (void)

header :: String
header = "Usage: bfi [options] [file]"

version :: Float
version = 0.1

enter :: String
enter = "Brainfuck interpreter REPL version " ++ show version ++ "\n"


flags :: [OptDescr String]
flags = [Option "i" ["interactive"]
         (NoArg "i") "Invoke REPL"
       , Option "t" ["tape"]
         (ReqArg id "(left, right)") "Start with tape (left, right)"]

commands :: String
commands = unlines ["Commands:"
                  , ":q\tExit"
                  , ":t\tPrint tape"
                  , ":c\tPrint column"
                  , ":i\tClear column and tape"
                  , ":h\tDisplay this message"]


init' :: [a] -> [a]
init' [] = []
init' x = init x

readFile' :: [String] -> IO String
readFile' [] = return ""
readFile' (x : _) = readFile x


readTape :: Tape -> Tape
readTape (left, right) = (right ++ repeat 0, reverse left)

showTape :: (Tape, Int) -> ([Int], Int, [Int])
showTape ((t : a, pe), acc) = (reverse pe
                              , t
                              , (concat . init' . group . take acc) a)
showTape _ = error "Can't show tape"

getTape :: [String] -> Tape
getTape [] = startTape
getTape (x : _) = readTape $ read x


repl :: (Tape, Int) -> IO ()
repl old@(tape, acc) = do
    input <- readline "> "
    case input of
        Just ":q" -> do
                putStrLn $ "Exiting at " ++ show acc
                         ++ " with tape " ++ show (showTape old)
                return ()
        Just ":t" -> do
                putStrLn $ "Tape at "
                         ++ show acc ++ " is: " ++ show (showTape old)
                repl old
        Just ":c" -> putStrLn ("Column " ++ show acc) >> repl old
        Just ":i" -> repl (startTape, 0)
        Just ":h" -> putStrLn commands >> repl old
        Just ":help" -> putStrLn commands >> repl old
        Nothing -> return ()
        Just x -> do
                addHistory x
                next <- interpret x tape acc
                putStr "\n"
                repl next

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    argv <- getArgs
    case getOpt Permute flags argv of
        ([], [], []) -> error $ usageInfo header flags
        (args, x, []) ->
            let tape = getTape $ filter (/= "i") args
            in do
                file <- readFile' x
                if "i" `elem` args then do
                    (tape', acc) <- interpret file tape 0
                    putStr enter
                    putStr $ "Entering REPL with tape: "
                           ++ show (showTape (tape', 500 + acc))
                    putChar '\n'
                    repl (tape', acc)
                    else void $ interpret file tape 0
        (_, _, err) -> error $ concat err ++ usageInfo header flags
