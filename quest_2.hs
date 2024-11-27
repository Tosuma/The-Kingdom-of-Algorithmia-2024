import Data.Char (isLetter, toUpper)
import Data.List ( nub, isPrefixOf )
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches(..))

main :: IO ()
main = do
    input1 <- readFile "./input-files/q2_1.txt"
    putStr "Q2 part 1: "
    print (part1 input1)


    input2 <- readFile "./input-files/q2_2.txt"
    putStr "Q2 part 2: "
    print (part2 input2)

        



{- Part 1 -}
part1 :: String -> Int
part1 input =
    let (wordsLine:sentenceLines) = lines input
        runes = parseWords wordsLine
        sentence = unwords sentenceLines
    in countRunicWords' sentence runes 

parseWords :: String -> [String]
parseWords line = splitOnComma $ drop 6 line


splitOnComma :: String -> [String]
splitOnComma s = case break (==',') s of
  (before, ',':after) -> before : splitOnComma after
  (before, "")        -> [before]
  _                   -> [s]


countRunicWords :: String -> [String] -> Int
countRunicWords sentence wordsList =
  sum [1 | i <- [0 .. length sentence - 1], w <- wordsList, w `isPrefixOf` drop i sentence]


countRunicWords' :: String -> [String] -> Int
countRunicWords' sentence wordsList =
  sum [countMatches w sentence | w <- wordsList]


countMatches :: String -> String -> Int
countMatches pattern target = length matches
  where
    matches = getAllTextMatches (target =~ pattern :: AllTextMatches [] String)


{- Part 2 -}
part2 :: String -> Int
part2 input = 
    let (wordsLine:sentenceLines) = lines input
        runes = parseWords wordsLine
    in sum [length $ getIndices runes s | s <- sentenceLines]


getIndices :: [String] -> String -> [Int]
getIndices searchTerms searchString =
    let lenString = length searchString
        indicesList = [ [i..(i + lenTerm - 1)] |
                        term <- searchTerms,
                        let lenTerm = length term,
                        let termRev = reverse term,
                        i <- [0..(lenString - lenTerm)],
                        let substr = take lenTerm (drop i searchString),
                        substr == term || substr == termRev ]
        indices = concat indicesList
    in nub indices



{- Part 3 -}
