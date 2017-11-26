--Solution 6.1

type Wahrheitswert = Bool

data VName = N1 | N2 | N3 | N4 | N5 deriving (Eq,Ord,Enum,Show)

newtype Variable = Var VName deriving (Eq,Ord,Show)

instance Enum Variable where
  fromEnum (Var name) = fromEnum name
  toEnum n = Var (toEnum n :: VName)


data Ausdruck = K Wahrheitswert
		| V Variable
		| Nicht Ausdruck
		| Und Ausdruck Ausdruck
		| Oder Ausdruck Ausdruck
        | Impl Ausdruck Ausdruck
        | Esgibt Variable Ausdruck
        | Fueralle Variable Ausdruck 
		deriving (Eq,Show)

type Belegung = Variable -> Wahrheitswert -- Total definierte Abbildung

test :: Belegung
test (Var N1) = True
test (Var N2) = False
test (Var N3) = True
test (Var N4) = False
test (Var N5) = True

evaluiere :: Ausdruck -> Belegung -> Wahrheitswert
evaluiere (K True)  f = True
evaluiere (K False) f = False
evaluiere (V var1) f = f var1
evaluiere (Nicht aus) f  = not $ evaluiere aus f
evaluiere (Und aus1 aus2) f = (evaluiere aus1 f) && (evaluiere aus2 f)
evaluiere (Oder aus1 aus2) f = (evaluiere aus1 f) || (evaluiere aus2 f) 
evaluiere (Impl aus1 aus2) f = (evaluiere (Nicht aus1) f) || (evaluiere aus2 f)
evaluiere (Esgibt var1 aus) f = (evaluiere aus (f1 f var1)) || (evaluiere aus (f2 f var1))
    where 
        f1 :: Belegung -> Variable -> Belegung
        f1 f var1 var2  
            | var1 == var2 = True
            | otherwise = (f var2)

        f2 :: Belegung -> Variable -> Belegung
        f2 f var1 var2  
            | var1 == var2 = False
            | otherwise = (f var2)
evaluiere (Fueralle var1 aus) f = (evaluiere aus (f1 f var1)) && (evaluiere aus (f2 f var1))
    where 
        f1 :: Belegung -> Variable -> Belegung
        f1 f var1 var2  
            | var1 == var2 = True
            | otherwise = (f var2)

        f2 :: Belegung -> Variable -> Belegung
        f2 f var1 var2  
            | var1 == var2 = False
            | otherwise = (f var2)
