-- Daniel Stanek 1225178
type Zahlenliste = [Integer]
type Tripelprimzahl = Integer

-- Exercise 3

-- Solution 1.1
schuerfen :: Zahlenliste -> [Tripelprimzahl]
schuerfen [] = []
schuerfen n = filter (contains primeTripel) (filter (>= head primeTripel) n)
        where contains :: [Integer] -> Integer -> Bool
              contains ns a = last (takeWhile (<= a) ns) == a
        
-- Helper 1.1
primeTripel :: [Integer]
primeTripel = zipWith (*) primes (zipWith (*)  (tail primes) (tail (tail primes)))

primes :: [Integer]
primes = 2 : 3 : filter (isPrime primes) [5, 7..]

isPrime :: [Integer] -> Integer -> Bool
isPrime (p:ps) n
         | p*p > n = True 
         | otherwise = n `rem` p /= 0 && isPrime ps n

-- Solution 1.4
verflechten3 :: [Int] -> [Int] -> [Int] -> [Int]
verflechten3 [] [] [] = []
verflechten3 [] bs cs = verflechten bs cs 
verflechten3 as [] cs = verflechten as cs 
verflechten3 as bs [] = verflechten as bs 
verflechten3 (a:as) (b:bs) (c:cs) = a : b : c : (verflechten3 as bs cs)

-- Helper 1.4
verflechten :: [Int] -> [Int] -> [Int]
verflechten (n1:n1s) (n2:n2s) = n1 : n2 : (verflechten n1s n2s)
verflechten [] (n2s) = n2s
verflechten (n1s) [] = n1s
