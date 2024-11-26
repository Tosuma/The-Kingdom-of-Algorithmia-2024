main :: IO ()
main = do
    input1 <- readFile "./input-files/q1_1.txt"
    putStr "Q1 part 1: "
    print (part1 input1)

    input2 <- readFile "./input-files/q1_2.txt"
    putStr "Q1 part 2: "
    print (part2 input2)
    
    input3 <- readFile "./input-files/q1_3.txt"
    putStr "Q1 part 3: "
    print (part3 input3)


test1 = "ABBAC"

part1 :: String -> Int
part1 s = sum $ map cost s

part1' :: String -> Int
part1' s = sum [cost c | c <- s]

cost :: Char -> Int
cost c
    | c == 'B' = 1
    | c == 'C' = 3
    | c == 'D' = 5
    | otherwise = 0


test2 = "AxBCDDCAxD"

part2 :: String -> Int
part2 []       = 0
part2 ('x':b:cs) = cost b + part2 cs
part2 (a:'x':cs) = cost a + part2 cs
part2 (a:b:cs) = cost a + cost b + 2 + part2 cs



test3 = "xBxAAABCDxCC"

part3 :: String -> Int
part3 []           = 0
part3 ('x':b:c:cs) = part2 [b, c] + part3 cs
part3 (a:'x':c:cs) = part2 [a, c] + part3 cs
part3 (a:b:'x':cs) = part2 [a, b] + part3 cs
part3 (a:b:c:cs)   = cost a + cost b + cost c + 6 + part3 cs

