type Wahrheitswert = Bool

data Name = N1 | N2 | N3 | N4 | N5 deriving (Eq,Ord,Enum,Show)

newtype Variable = Var Name deriving (Eq,Ord,Show)

data Ausdruck = K Wahrheitswert
		| V Variable
		| Nicht Ausdruck
		| Und Ausdruck Ausdruck
		| Oder Ausdruck Ausdruck
		deriving (Eq,Show)

type Belegung = Variable -> Wahrheitswert -- Total definierte Abbildung

auswerten :: Ausdruck -> Variable -> Wahrheitswert -> Wahrheitswert
auswerten (K True)  _ _ = True
auswerten (K False) _ _ = False
auswerten (V var1) var2 True
	| var1 == var2 = True
auswerten (V var1) var2 False 
	| var1 == var2 = False
auswerten (Nicht aus) var1 b = not $ auswerten aus var1 b
auswerten (Und aus1 aus2) var1 b = (auswerten aus1 var1 b) && (auswerten aus2 var1 b)
auswerten (Oder aus1 aus2) var1 b = (auswerten aus1 var1 b) || (auswerten aus2 var1 b) 