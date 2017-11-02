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
