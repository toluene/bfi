module Brainfuck (interpret, startTape, Tape) where

import Data.Char (ord, chr)

type Tape = ([Int], [Int])

startTape :: Tape
startTape = (repeat 0, [])

interpret :: String -> Tape -> Int -> IO (Tape, Int)
interpret [] tape acc = return (tape, acc)
interpret ('<' : xs) (ys, rest) acc = case rest of
    (r : est) -> interpret xs (r : ys, est) (succ acc)
    [] -> return ((0 : ys, []), acc)
interpret (x : xs) tape@(y : ys, rest) acc = case x of
    '+' -> interpret xs (succ y : ys, rest) (succ acc)
    '-' -> interpret xs (pred y : ys, rest) (succ acc)
    '.' -> putChar (chr y) >> interpret xs tape (succ acc)
    ',' -> getChar
           >>= (\ z -> interpret xs (ord z : ys, rest) (succ acc))
    '[' -> interpretLoop xs tape (succ acc)
           >>= (\ (xs', tape', acc') -> interpret xs' tape' acc')
    '>' -> interpret xs (ys, y : rest) (succ acc)
    _ -> interpret xs tape (succ acc)
interpret _ _ acc = error $ "Tape error at " ++ show acc

interpretLoop :: String -> Tape -> Int -> IO (String, Tape, Int)
interpretLoop ys tape acc =
    if ((==) 0 . head . fst) tape
        then return (rest', tape, acc + length foundBrackets)
    else do
        (tape', _) <- interpret foundBrackets tape acc
        interpretLoop ys tape' acc
    where findMatchingBracket :: String -> Int -> String -> (String, String)
          findMatchingBracket (']' : xs) 0 text = (text, xs)
          findMatchingBracket (x : xs) n text = case x of
              '[' -> findMatchingBracket xs (succ n) (text ++ "[")
              ']' -> findMatchingBracket xs (pred n) (text ++ "]")
              _ -> findMatchingBracket xs n (text ++ [x])
          findMatchingBracket [] _ _ = error $ "Bracket unmatched at "
                                       ++ show acc
          (foundBrackets, rest') = findMatchingBracket ys 0 ""
