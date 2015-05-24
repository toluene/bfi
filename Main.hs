import Brainfuck
import System.Console.Readline
import System.Console.GetOpt
import System.Environment (getArgs)
import Data.List (group)
import Control.Monad (void)

header :: String
header = "Usage: bfi [options] [file]"

version :: Float
version = 0.1

enter :: String
enter = "Brainfuck interpreter REPL version " ++ show version ++ "\n"

readTape :: Tape -> Tape
readTape (left, right) = (right ++ repeat 0, reverse left)

main :: IO ()
main = do
    argv <- getArgs
    case getOpt Permute flags argv of
        ([], x : _, []) -> do
            file <- readFile x
            void $ interpret file startTape 0
        ([], [], []) -> error $ usageInfo header flags
        ("i" : [], [], []) -> do
            putStr enter
            putStr $ "Entering REPL with tape: "
                   ++ show (showTape (startTape, 0))
            putChar '\n'
            repl (startTape, 0)
        (args, [], []) ->
            if "i" `elem` args then
                let tape = readTape $ read $ concat $ filter (/= "i") args
                in do
                    putStr enter
                    putStr $ "Entering REPL with tape: "
                           ++ show (showTape (tape, 0))
                    putChar '\n'
                    repl (tape, 0)
            else error $ usageInfo header flags
        (args, x : _, []) -> 
            let tape = readTape $ read $ concat $ filter (/= "i") args in
                if "i" `elem` args then do
                    file <- readFile x
                    a <- interpret file tape 0
                    putChar '\n'
                    putStr enter
                    putStr $ "Entering REPL with tape: " ++ show (showTape a)
                    putChar '\n'
                    repl a
                else do
                    file <- readFile x
                    void $ interpret file tape 0
        (_, _, err) -> error $ concat err ++ usageInfo header flags

flags :: [OptDescr String]
flags = [Option "i" ["interactive"] (NoArg "i") "Invoke REPL"
       , Option "t" ["tape"] (ReqArg id "(left, right)") "Start with tape (left, right)"]

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
        Just ":c" -> do
                putStrLn $ "Column " ++ show acc
                repl old
        Just ":i" -> repl (tape, 0)
        Just ":h" -> putStrLn commands >> repl old
        Just ":help" -> putStrLn commands >> repl old
        Nothing -> return ()
        Just x -> do
                addHistory x
                next <- interpret x tape acc
                putStr "\n"
                repl next

commands :: String
commands = unlines ["Commands:"
                  , ":q\tExit"
                  , ":t\tPrint tape"
                  , ":c\tPrint column"
                  , ":i\tClear column"
                  , ":h\tDisplay this message"]

init' :: [a] -> [a]
init' [] = []
init' x = init x

showTape :: (Tape, Int) -> ([Int], Int, [Int])
showTape ((t : a, pe), acc) = (reverse pe
                              , t
                              , (concat . init' . group . take acc) a)
showTape _ = error "Can't show tape"
