Daniel Stanek 1225178

>type Zahlenliste = [Integer]
>type Tripelprimzahl = Integer

Exercise 3

Solution 1.1

>schuerfen :: Zahlenliste -> [Tripelprimzahl]
>schuerfen [] = []
>schuerfen n = filter (contains primeTripel) (filter (>= head primeTripel) n)
>        where contains :: [Integer] -> Integer -> Bool
>              contains ns a = last (takeWhile (<= a) ns) == a
        
Helper 1.1

>primeTripel :: [Integer]
>primeTripel = zipWith (*) primes (zipWith (*)  (tail primes) (tail (tail primes)))

>primes :: [Integer]
>primes = 2 : 3 : filter (isPrime primes) [5, 7..]

>isPrime :: [Integer] -> Integer -> Bool
>isPrime (p:ps) n
>         | p*p > n = True 
>         | otherwise = n `rem` p /= 0 && isPrime ps n

Solution 1.4

>newtype Kurs = K Float deriving (Eq, Ord, Show)
>instance Num Kurs where 
>       (+) (K x) (K y) = K (x + y)
>       (*) (K x) (K y) = K (x * y)
>       abs (K x) 
>           | x > 0 = K x
>           | otherwise = K (negate x)
>       signum (K x)
>           | x > 0 = 1
>           | x == 0 = 0
>           | x < 0 = -1
>       fromInteger x = K (fromInteger x)
>       negate (K x) = K (0 - x)

>newtype Pegelstand = Pgl Float deriving (Eq, Ord, Show)
>instance Num Pegelstand where 
>       (+) (Pgl x) (Pgl y) = Pgl (x + y)
>       (*) (Pgl x) (Pgl y) = Pgl (x * y)
>       abs (Pgl x) 
>           | x > 0 = Pgl x
>           | otherwise = Pgl (negate x)
>       signum (Pgl x)
>           | x > 0 = 1
>           | x == 0 = 0
>           | x < 0 = -1
>       fromInteger x = Pgl (fromInteger x)
>       negate (Pgl x) = Pgl (0 - x)


Solution 1.3

>curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
>curry3 f x y z = f (x, y, z)

>uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
>uncurry3 g(x,y,z) = g x y z

>curry_flip :: ((a,b) -> c) -> (b -> a -> c)
>curry_flip f x y = f (y, x)

>uncurry_flip :: (a -> b -> c) -> ((b,a) -> c)
>uncurry_flip f (x, y) = f y x

Helper 1.3

>binom :: (Int, Int) -> [Int]
>binom (a,b) = [div (fac a) ((fac b) * (fac (a - b)))]

>fac :: Int -> Int
>fac n = product [1..n]

>eucLengthSquare :: (Int, Int, Int) -> [Int]
>eucLengthSquare (x,y,z) = [(x*x + y*y + z*z)]

Example 1.3

>test :: Int -> [Int]
>test 1 = uncurry3 verflechten3 ([1,4,7],[2,5,8],[3,6,9])
>test 2 = uncurry_flip verflechten ([2,4,6], [1,3])
>test 3 = curry_flip binom 10 20
>test 4 = curry3 eucLengthSquare 1 2 3

Solution 1.4

>verflechten3 :: [Int] -> [Int] -> [Int] -> [Int]
>verflechten3 [] [] [] = []
>verflechten3 [] bs cs = verflechten bs cs 
>verflechten3 as [] cs = verflechten as cs 
>verflechten3 as bs [] = verflechten as bs 
>verflechten3 (a:as) (b:bs) (c:cs) = a : b : c : (verflechten3 as bs cs)

Helper 1.4

>verflechten :: [Int] -> [Int] -> [Int]
>verflechten (n1:n1s) (n2:n2s) = n1 : n2 : (verflechten n1s n2s)
>verflechten [] (n2s) = n2s
>verflechten (n1s) [] = n1s
