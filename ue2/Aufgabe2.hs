import Data.List (sortBy)

type Nat0               =   Integer
type Nat1               =   Integer
type GesamtKugelZahl    =   Nat1
type GezogeneKugelZahl  =   Nat1
type Spiel              =   (GesamtKugelZahl, GezogeneKugelZahl)
type Gluecksspiel       =   (Spiel, Spiel)
type AngeboteneSpiele   =   [Gluecksspiel]

-- Exercise 2


-- Solution 2.1
-- First entry is the lowest
-- lcm computs the largest common multiple
p2p :: (Nat0, Nat0) -> (Nat0, Nat0)
p2p (m,n) = (p,q)
    where p = (lcm m n) `div` m 
          q = (lcm m n) `div` n


-- Solution 2.2
-- Use a function to filter out the games with combinations equal to 0
-- Compare the games to determine the order
attraktiveSpieleVorne :: AngeboteneSpiele -> [Gluecksspiel]
attraktiveSpieleVorne xs = filter loescheSpiele (sortBy vergleicheSpiele xs)
    where vergleicheSpiele a b 
                    | anzahlWettKombis a == anzahlWettKombis b = snd(b) `compare` snd(a)
                    | otherwise                                = anzahlWettKombis a `compare` anzahlWettKombis b
          loescheSpiele a 
                    | anzahlWettKombis a == 0 = False
                    | otherwise               = True

-- Helper 2.2 from Exercise 1
anzahlWettKombis    ::  Gluecksspiel -> Nat0
anzahlWettKombis inputSpiel = binom(fst(inputSpiel)) * binom(snd(inputSpiel))

-- Declare Binomial Coefficient Function
-- for more readability
binom :: (Nat0,Nat0) -> Nat0
binom (a,b) = div (fac a) ((fac b) * (fac (a - b)))

-- Declare Factorial function with Product 
-- of a list for simplicity
fac :: Nat0 -> Nat0
fac n = product [1..n]


-- Solution 2.3

type Toepfchen = [Int]
type Kroepfchen = [Int]
type Zahlenliste = [Int]

-- Define local scoped functions for more readability
aufteilen :: Zahlenliste -> (Toepfchen, Kroepfchen)
aufteilen ns = (gs,ss)
    where gs = [n | n <- ns, (modSum (sumOnes (baseTransform n)))] 
          ss = [n | n <- ns, (not (modSum (sumOnes (baseTransform n))))]
          sumOnes xs = sum (filter (== 1) xs)
          modSum res 
                | res `mod` 3 == 0 = True
                | otherwise = False

-- Helper 2.3
-- Declare a function to transform numbers into 3-base
baseTransform :: Int  -> [Int]
baseTransform n 
    | n == 0 = [0]
    | n `mod` 3 == 2 = 2 : baseTransform (n `div` 3)
    | n `mod` 3 == 1 = 1 : baseTransform (n `div` 3)
    | n `mod` 3 == 0 = 0 : baseTransform (n `div` 3) 

-- Solution 2.4
type Nat = [Int]

istGueltig :: Nat -> Bool
istGueltig (ns)
    | ns == [0] = True
    | (filter (> 9) ns) ++ (filter (< 0) ns) == [] = True
    | otherwise = False 

normalForm :: Nat -> Nat
normalform (n:[]) = [n]
normalForm (n:ns)
    | not (istGueltig (n:ns)) = []
    | n == 0 = normalForm ns
    | otherwise = n:ns 

addiere :: Nat -> Nat -> Nat
addiere as bs
    | not (istGueltig as) || not (istGueltig bs) = []
    | otherwise = normalForm (reverseList (sumUp sa sb))
    where sa = reverseList (fillZeros as maxLength)
          sb = reverseList (fillZeros bs maxLength)
          maxLength  
            | length as > length bs = length as + 1
            | otherwise = length bs + 1

          sumUp :: Nat -> Nat -> Nat
          sumUp [] [] = [] 
          sumUp (a:as) (b:bs)
                | (a + b) > 9 = (a + b - 10) : sumUp (((head as) + 1) : tail as) bs
                | otherwise = (a + b) : sumUp as bs  

subtrahiere :: Nat -> Nat -> Nat
subtrahiere [] bs = []
subtrahiere as [] = []
subtrahiere as bs
    | not (istGueltig as) || not (istGueltig bs) = []
    | preCheck as bs = [0] 
    | otherwise = normalForm (reverseList (diffDown sa sb))
    where sa = reverseList (fillZeros as maxLength)
          sb = reverseList (fillZeros bs maxLength)
          maxLength
            | length as > length bs = length as + 1
            | otherwise = length bs + 1

          diffDown :: Nat -> Nat -> Nat
          diffDown [] [] = []
          diffDown (a:as) (b:bs)
            | (a - b) < 0 = (a - b + 10) : diffDown (((head as) - 1) : tail as) bs
            | otherwise = (a - b) : diffDown as bs  

          preCheck :: Nat -> Nat -> Bool
          preCheck as bs 
            | length bs > length as = True
            | length bs == length as && head bs > head as = True
            | length bs == length as && head bs == head as = preCheck (tail as) (tail bs)
            | otherwise = False

fillZeros :: Nat -> Int -> Nat
fillZeros as n
    | diffLength < 0 = as
    | otherwise = (take diffLength (repeat 0)) ++ as
    where diffLength = n - length as 

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
