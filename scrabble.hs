import Data.Char
import Data.List
import System.Environment
import System.IO

main = do args <- getArgs
          case args of
               [filename] ->
                   do file <- readFile filename
                      let wordList = lines file
                          filtered = filter (all isLower) wordList
                          filtered' = filter (\x -> length x > 1) filtered
                          dict = map latinify filtered'
                          in do interactLines $ searcher dict
               otherwise ->
                   do progName <- getProgName
                      putStrLn ("Usage: " ++ progName ++ " words-file")

interactLines f = do putStr "> "
                     hFlush stdout
                     line <- getLine
                     if line == ""
                        then return ()
                        else do putStr $ f line
                                interactLines f

searcher :: [String] -> String -> String
searcher dict tiles =
    let anagrams = findAnagrams tiles dict
        sorted = sortBy (\x y -> compare (score x) (score y)) $ anagrams
        annotated = map (\x -> (show (score x)) ++ "\t" ++ x) sorted
        in (unlines annotated)

latinify = map latinifyChar
            where latinifyChar 'é' = 'e'
                  latinifyChar 'ñ' = 'n'
                  latinifyChar 'ç' = 'c'
                  latinifyChar 'å' = 'a'
                  latinifyChar 'ö' = 'o'
                  latinifyChar 'è' = 'e'
                  latinifyChar 'ó' = 'o'
                  latinifyChar 'ê' = 'e'
                  latinifyChar 'û' = 'u'
                  latinifyChar 'ä' = 'a'
                  latinifyChar 'â' = 'a'
                  latinifyChar x = x
isAlphabetic = all (\x -> ord x >= ord 'a' && ord x <= ord 'z')

findAnagrams :: String -> [String] -> [String]
findAnagrams tiles words = let letterBag = sort tiles
                               (dots, letters) = span (== '.') letterBag
                               ndots = length dots
                               raw = filter (isAnagram ndots letters) words
                               in map (processDots letters) raw

-- sorted_letters -> word -> word_with_wildcards_capitalized
processDots :: String -> String -> String
processDots tiles (w:word) = let (found, removed) = removeOne w tiles
                                 rest = processDots removed word
                                 in if found
                                       then w:rest
                                       else (toUpper w):rest
processDots _ [] = []

removeOne :: Char -> String -> (Bool, String)
removeOne c (x:xs) | c == x    = (True, xs)
                   | otherwise = let (found, removed) = removeOne c xs
                                 in (found, x:removed)
removeOne c [] = (False, [])

-- num_wildcards -> sorted_letters -> word -> is_anagram
isAnagram :: Int -> String -> String -> Bool
isAnagram ndots letters word =
    isSubSequenceOf ndots (sort word) letters

-- num_wildcards -> sub_sequence_candidate -> sequence -> is_subsequence
isSubSequenceOf :: Int -> String -> String -> Bool
isSubSequenceOf _ [] _ = True
isSubSequenceOf 0 (x:xs) [] = False
isSubSequenceOf n (x:xs) [] = isSubSequenceOf (n-1) xs []
isSubSequenceOf n (x:xs) (y:ys) | x == y    = isSubSequenceOf n xs ys
                                | n > 0     = isSubSequenceOf n (x:xs) ys ||
                                              isSubSequenceOf (n-1) xs ys
                                | otherwise = isSubSequenceOf 0 (x:xs) ys

score word = sum $ map leterScore word
leterScore x = case x of
               'a' -> 1
               'b' -> 3
               'c' -> 3
               'd' -> 2
               'e' -> 1
               'f' -> 4
               'g' -> 2
               'h' -> 4
               'i' -> 1
               'j' -> 8
               'k' -> 5
               'l' -> 1
               'm' -> 3
               'n' -> 1
               'o' -> 1
               'p' -> 3
               'q' -> 10
               'r' -> 1
               's' -> 1
               't' -> 1
               'u' -> 1
               'v' -> 4
               'w' -> 4
               'x' -> 8
               'y' -> 4
               'z' -> 10
               otherwise -> 0
