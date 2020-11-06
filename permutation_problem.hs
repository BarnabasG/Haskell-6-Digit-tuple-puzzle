import Control.Monad ( replicateM )

{-*Part 1*-}
rule1 :: Eq a => (a,a,a,a,a,a) -> Bool
rule1 (a1,a2,a3,a4,a5,a6)                           -- Returns true if all digits are unique
    =   unique (tupleConv (a1,a2,a3,a4,a5,a6)) == True

{-*Part 2*-}
rule2 :: (Int,Int,Int,Int,Int,Int) -> Bool          -- Returns true if all alternate digits
rule2 (a1,a2,a3,a4,a5,a6)                           -- have differing parity
    =   alternateMod (tupleConv (a1,a2,a3,a4,a5,a6)) == True

{-*Part 3*-}
rule3 :: (Int,Int,Int,Int,Int,Int) -> Bool          -- Returns true if all alternate digits
rule3 (a1,a2,a3,a4,a5,a6)                           -- differ by more than 2
    =   alternateAbs (tupleConv (a1,a2,a3,a4,a5,a6)) == True

{-*Part 4*-}
rule4 :: (Int,Int,Int,Int,Int,Int) -> Bool          -- Returns true if the first and middle pairs
rule4 (a1,a2,a3,a4,a5,a6)                           -- of numbers are multiples of the last
    =   areFactors (tupleConv (a1,a2,a3,a4,a5,a6)) == True

possibles :: [(Int,Int,Int,Int,Int,Int)]            -- Gives a list of all possible tuples 
                                                    -- representing six-digit numbers
possibles
    = map listConv possiblesList

isSolution :: (Int,Int,Int,Int,Int,Int) -> Bool     -- Checks if all the rule
isSolution (i1,i2,i3,i4,i5,i6)                      -- conditions are met, and returns
    |   rule1 (i1,i2,i3,i4,i5,i6) == True           -- a Bool
        &&  rule2 (i1,i2,i3,i4,i5,i6) == True
        &&  rule3 (i1,i2,i3,i4,i5,i6) == True
        &&  rule4 (i1,i2,i3,i4,i5,i6) == True  = True
    |   otherwise       = False

unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs)
    |   x `elem` xs     = False             -- Is x also an element of xs
    |   otherwise       = unique xs

alternateMod :: [Int] -> Bool               -- Tests for each x that it's parity is
alternateMod [_] = True                     -- different to that of head xs
alternateMod (x:xs)
    |   (\n -> n `mod` 2) x == (\n -> n `mod` 2) (head xs) = False
    |   otherwise = alternateMod xs

alternateAbs :: [Int] -> Bool               -- Tests for each x that it is at least
alternateAbs [_] = True                     -- +- 3 from xs
alternateAbs (x:xs)
    |   abs (x - (head xs)) < 3 = False
    |   otherwise = alternateAbs xs

areFactors :: [Int] -> Bool                 -- Convert to integers to a 2-digit integer and
areFactors (_:_:[]) = True                  -- use lambda expression to determine if factor
areFactors (x:y:xs)
    |   (\x -> \n -> n `mod` x /= 0) ((\x -> \n -> (10*x) + n) 
        (lastButOne xs) (last xs)) ((\x -> \n -> (10*x) + n) x y)
        = False
    |   otherwise = areFactors xs

lastButOne :: [Int] -> Int                  -- Find second to last element in list
lastButOne (x:_:[]) = x
lastButOne (_:xs)
    = lastButOne xs
    
possiblesList :: [[Int]]
possiblesList
    = replicateM 6 [0..9]                   -- Using Control.Monad

listConv :: [a] -> (a,a,a,a,a,a)            -- Convert a list to a tuple
listConv [a,b,c,x,y,z] = (a,b,c,x,y,z)      -- Polymorphic

tupleConv :: (a,a,a,a,a,a) -> [a]           -- Convert a tuple to a list
tupleConv (a,b,c,x,y,z) = [a,b,c,x,y,z]     -- Polymorphic


main :: IO ()
main
    = putStrLn (show (filter isSolution possibles))
