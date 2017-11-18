Solution 4.1

>data Nat = Null | N Nat

>instance Show Nat where
>	show Null = "0"
>	show (N Null) = "1"
>	show n = show (div2 n) ++ show (mod2 n)
>		where
>			numN :: Nat -> Int
>			numN Null = 0
>			numN (N n) = 1 + numN n
>
>			natN :: Int -> Nat
>			natN n
>				| n == 0 = Null
>				| n > 0 = N $ natN (n - 1)

>			mod2 :: Nat -> Int
>			mod2 n = (numN n) `mod` 2

>			div2 :: Nat -> Nat
>			div2 n = natN $ ((numN n) - (mod2 n)) `div` 2

>instance Eq Nat where
>	Null == Null = True
>	(N n) == Null = False
>	Null == (N n) = False
>	(N n1) == (N n2) = n1 == n2 

>instance Ord Nat where 
>	Null <= Null = True
>	Null <= (N n) = True
>	(N n) <= Null = False
>	(N n1) <= (N n2) = n1 <= n2

>instance Num Nat where 
>	(+) n Null = n
>	(+) n1 (N n2) = (+) (N n1) n2 
>	(*) n Null = Null
>	(*) Null n = NullQQ
>	(*) n1 (N n2) = iterate (n1 +) n1 !! (fromEnum n2)
>	(-) n Null = n
>	(-) Null n = (-) n Null
>	(-) (N n1) (N n2) = (-) n1 n2
>	abs n = n
>	signum Null = Null
>	signum n = (N Null)
>	fromInteger n
>		| n == 0 = Null
>		| n > 0 = N $ fromInteger (n - 1)

>instance Enum Nat where
>	fromEnum Null = 0
>	fromEnum (N n) = 1 + fromEnum n
>	toEnum n
>		| n == 0 = Null
>		| n > 0 = N $ toEnum (n - 1)



Solution 4.2

>type Wahrheitswert = Bool

>data Name = N1 | N2 | N3 | N4 | N5 deriving (Eq,Ord,Enum,Show)

>newtype Variable = Var Name deriving (Eq,Ord,Show)

>instance Enum Variable where
>  fromEnum (Var name) = fromEnum name
>  toEnum n = Var (toEnum n :: Name)


>data Ausdruck = K Wahrheitswert
>		| V Variable
>		| Nicht Ausdruck
>		| Und Ausdruck Ausdruck
>		| Oder Ausdruck Ausdruck
>		deriving (Eq,Show)

>type Belegung = Variable -> Wahrheitswert -- Total definierte Abbildung

>test :: Belegung
>test (Var N1) = True
>test (Var N2) = False
>test (Var N3) = True
>test (Var N4) = False
>test (Var N5) = True

>auswerten :: Ausdruck -> Belegung -> Wahrheitswert
>auswerten (K True)  f = True
>auswerten (K False) f = False
>auswerten (V var1) f = f var1
>auswerten (Nicht aus) f  = not $ auswerten aus f
>auswerten (Und aus1 aus2) f = (auswerten aus1 f) && (auswerten aus2 f)
>auswerten (Oder aus1 aus2) f = (auswerten aus1 f) || (auswerten aus2 f) 
