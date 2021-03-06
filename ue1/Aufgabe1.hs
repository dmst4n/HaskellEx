type Nat0               =   Integer
type Nat1               =   Integer
type GesamtKugelZahl    =   Nat1
type GezogeneKugelZahl  =   Nat1
type Spiel              =   (GesamtKugelZahl, GezogeneKugelZahl)
type Gluecksspiel       =   (Spiel, Spiel)

-- Exercise 1.1

-- Solution 1.1
anzahlWettKombis    ::  Gluecksspiel -> Nat0
anzahlWettKombis inputSpiel = binom(fst(inputSpiel)) * binom(snd(inputSpiel))


-- Helper 1.1

-- Declare Binomial Coefficient Function
-- for more readability
binom :: (Nat0,Nat0) -> Nat0
binom (a,b) = div (fac a) ((fac b) * (fac (a - b)))

-- Declare Factorial function with Product 
-- of a list for simplicity
fac :: Nat0 -> Nat0
fac n = product [1..n]



-- Exercise 1.2

-- Solution 1.2
-- Delete 1 because Fibonacci Sequence 
-- in this program starts with 0
-- If fib occurs two times, take lower index.
fib' :: Nat0 -> Nat0
fib' n 
    | n == last fibT = toNat (length fibT) - 1 - checkMultiples fibT
    | otherwise = n
    where   fibT = [ x | x <- takeWhile (<= n) fib]
            checkMultiples xs 
                | last xs == last (init xs) = 1 + checkMultiples (init xs)
                | otherwise = 0 

-- Helper 1.2

-- Define Fibonacci sequence
-- zipWith to add up the rows
-- tail to always add all elements except for the head
-- starts with a leading 0 for logic reasons
fib :: [Nat0]
fib = 0 : 1 : zipWith (+) fib (tail fib) 

-- Define conversion from int to Nat0
toNat :: Int -> Nat0
toNat n = fromIntegral n



-- Exercise 1.3

-- Solution 1.3
-- Mostly defined in 1.2
-- Define a list which adds itself over and over again 
-- to achieve endless fibonacci sequence
fibs :: Nat0 -> [Nat0]
fibs n = take ((fromNat n) + 1) fib


-- Helper 1.3

-- Define cast from Nat0 to Int
fromNat :: Nat0 -> Int
fromNat n = fromIntegral n



-- Exercise 1.4

-- Solution 1.4
-- use pattern matching to end recursion
verflechten :: [Int] -> [Int] -> [Int]
verflechten (n1:n1s) (n2:n2s) = n1 : n2 : (verflechten n1s n2s)
verflechten [] (n2s) = n2s
verflechten (n1s) [] = n1s
