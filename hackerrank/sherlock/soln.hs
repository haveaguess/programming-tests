-- Enter your code here. Read input from STDIN. Print output to STDOUT

-- we know it has to be a multiple of 3 x '5' and 5 x '3' in the numbers
-- we also know that the fives have to be first for maximum numbers but let's not rely on that.. 
generateCandidates n = [ replicate (threes*5) '3' ++ replicate (fives*3) '5' | threes <- [0..n], fives <- [0..n] ]
generateNumbers n = map (read::String->Integer) $ filter ((n==) . length) (generateCandidates n)
generateMaxNumber n = if length (generateNumbers n) == 0 then "-1" else show $ maximum $ generateNumbers n
generateMaxNumberStr str = generateMaxNumber (read str :: Int)

-- pattern match first line as testCount and test cases as rest of lines
processInput (testCount:testCases) =
  if (read testCount) == length testCases
    -- if valid file
    then unlines $ map (generateMaxNumber . (read::String->Int)) testCases
    -- fail fast if file corrupt
    else error "test cases didnt match header count of file"

processInputLines string = processInput $ lines string

main = interact processInputLines