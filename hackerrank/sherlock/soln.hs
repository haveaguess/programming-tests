-- solution to the hackerrank problem : https://www.hackerrank.com/contests/sep13/challenges/sherlock-and-the-beast

-- we know it has to be a multiple of 3 x '5' and 5 x '3' in the numbers
-- we also know that the fives have to be first for maximum numbers but let's not rely on that.. 

generateDecentNumbers n = 
    [ replicate (n - threes) '5' ++ replicate threes '3' | threes <- [0..n], (n - threes) `mod` 3 + threes `mod` 5 == 0 ] 

generateMaxNumber n = if decentNumbers == [] then "-1" else decentNumbers !! 0
    where decentNumbers = generateDecentNumbers n 

generateMaxNumberStr str = generateMaxNumber (read str :: Int)

-- pattern match first line as testCount and test cases as rest of lines
processInput (testCount:testCases) =
  if (read testCount) == length testCases
    -- if valid file
    then unlines $ map generateMaxNumberStr testCases
    -- fail fast if file corrupt
    else error "test cases didnt match header count of file"

processInputLines string = processInput $ lines string

main = interact processInputLines