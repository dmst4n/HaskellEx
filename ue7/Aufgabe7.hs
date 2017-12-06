import Data.List (sort, sortBy, nub)
import Data.Ord (comparing)

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

ist_tautologie :: Ausdruck -> Ausdruck -> Bool
ist_tautologie aus1 aus2 = evaluiere (Fueralle (Var N1) (Fueralle (Var N2) (Fueralle (Var N3) (Fueralle (Var N4) (Fueralle (Var N5) (Oder (Und aus1 aus2) (Nicht (Und aus1 aus2)))))))) f
    where 
        f :: Belegung
        f _ = True

schreibe :: Ausdruck -> String
schreibe (K True) = "wahr"
schreibe (K False) = "falsch"
schreibe (V (Var n)) = show n
schreibe (Nicht a) = "(" ++ "neg" ++ " " ++ schreibe a ++ ")"
schreibe (Und a1 a2) = "(" ++ schreibe a1 ++ " " ++ "und" ++ " " ++ schreibe a2 ++ ")"
schreibe (Oder a1 a2) = "(" ++ schreibe a1 ++ " " ++ "oder" ++ " " ++ schreibe a2 ++ ")" 
schreibe (Impl a1 a2) = "(" ++ schreibe a1 ++ " " ++ "=>" ++ " " ++ schreibe a2 ++ ")" 
schreibe (Esgibt v a) = "(" ++ "EG" ++ " " ++  schreibe (V v) ++ "." ++ " " ++ schreibe a ++")"
schreibe (Fueralle v a) = "(" ++ "FA" ++ " " ++  schreibe (V v) ++ "." ++ " " ++ schreibe a ++")"

nnf :: Ausdruck -> Ausdruck 
nnf (Nicht (K True)) = (K False)
nnf (Nicht (K False)) = (K True)
nnf (Nicht (V (Var n))) = (Nicht (V (Var n)))
nnf (Nicht (Nicht a)) = nnf a
nnf (Nicht (Und a1 a2)) = (Oder (nnf (Nicht a1)) (nnf (Nicht a2)))
nnf (Nicht (Oder a1 a2)) = (Und (nnf (Nicht a1)) (nnf (Nicht a2)))
nnf (Nicht (Impl a1 a2)) = nnf $ Nicht $ Oder (nnf (Nicht a1)) (nnf a2)
nnf (Nicht (Esgibt v a)) = Fueralle v $ nnf $ Nicht a
nnf (Nicht (Fueralle v a)) = Esgibt v $ nnf $ Nicht a
nnf (K True) = (K True)
nnf (K False) = (K False)
nnf (V (Var n)) = (V (Var n))
nnf (Und a1 a2) = Und (nnf a1) (nnf a2)
nnf (Oder a1 a2) = Oder (nnf a1) (nnf a2)
nnf (Impl a1 a2) = Impl (nnf a1) (nnf a2)
nnf (Esgibt v a) = (Esgibt v (nnf a))
nnf (Fueralle v a) = (Fueralle v (nnf a)) 


type Nat0 = Int
type Nat1 = Int
type Anzahl_Blutkonserven = Nat0
type Reiseproviant = Anzahl_Blutkonserven
type Reisedauer = Nat1 -- In vollen Std., ausschliesslich Werte von 1..12
type Abfahrtszeit = Nat0 -- In vollen Std., ausschliesslich Werte von 0..23
type Ankunftszeit = Nat0 -- In vollen Std., ausschliesslich Werte von 0..23
data Stadt = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 deriving Show
type Ausgangsort = Stadt
type Zielort = Stadt
type Abfahrtsort = Stadt
type Ankunftsort = Stadt
type Relation = (Abfahrtsort,Abfahrtszeit,Ankunftsort,Ankunftszeit)
type Reiseplan = [Relation]
type Fahrplan = Abfahrtsort -> [(Ankunftsort,Abfahrtszeit,Reisedauer)] -- Total def.

instance Eq Stadt where
    S1 == S1 = True
    S2 == S2 = True
    S3 == S3 = True
    S4 == S4 = True
    S5 == S5 = True
    S6 == S6 = True
    S7 == S7 = True
    S8 == S8 = True
    S9 == S9 = True
    S10 == S10 = True
    _ == _ = False



fahrplan :: Fahrplan
fahrplan S1 = [(S3, 19, 2),(S5, 23, 3)]
fahrplan _ = []
