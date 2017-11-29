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

-- Exercise 6.2
type Nat1 = Int
type Name = String
type Alter = Nat1
data Geschlecht = M | W | X deriving Show
type Gemeinde = String
type Strasse = String
type Hausnr = Nat1
data Person = P Name Alter Geschlecht Wohnsitze deriving Show
data Anschrift = A Gemeinde Strasse Hausnr deriving Show
type Wohnsitze = [Anschrift]
type Melderegister = [Person]
data Registerbaum = Leer | Verzweigung Registerbaum Person Registerbaum deriving (Eq, Show)

instance Eq Geschlecht where
	M == M = True
	W == W = True
	X == X = True
	_ == _ = False

instance Ord Geschlecht where
	compare M W = GT 
	compare W X = GT
	compare M X = GT
	compare M M = EQ
	compare W W = EQ
	compare X X = EQ
	compare W M = LT 
	compare X W = LT 
	compare X M = LT

instance Eq Anschrift where
	(A gemeinde1 strasse1 hausnr1) == (A gemeinde2 strasse2 hausnr2) = (gemeinde1 == gemeinde2) && 
																	   (strasse1 == strasse2) &&
																	   (hausnr1 == hausnr2)

instance Ord Anschrift where
	compare (A gemeinde1 strasse1 hausnr1) (A gemeinde2 strasse2 hausnr2)
		| gemeinde1 /= gemeinde2 = compare gemeinde1 gemeinde2
		| gemeinde1 == gemeinde2 && strasse1 /= strasse2 = compare strasse1 strasse2
		| otherwise = compare hausnr1 hausnr2

instance Eq Person where
	(P name1 alter1 geschlecht1 ws1) == (P name2 alter2 geschlecht2 ws2) = (name1 == name2) && 
																		   (alter1 == alter2) && 
																		   (geschlecht1 == geschlecht2) &&
																		   ((sort ws1) == (sort ws2))

instance Ord Person where
	compare (P name1 alter1 geschlecht1 ws1) (P name2 alter2 geschlecht2 ws2)
		| name1 /= name2 = compare name1 name2
		| name1 == name2 && geschlecht1 /= geschlecht2 = compare geschlecht1 geschlecht2
		| otherwise = compare alter1 alter2


migration :: Melderegister -> Registerbaum
migration [] = Leer
migration ps = fillBaum Leer ps
    where
        fillBaum :: Registerbaum -> Melderegister -> Registerbaum
        fillBaum baum [] = baum
        fillBaum Leer (p:ps) = fillBaum (Verzweigung Leer p Leer) ps
        fillBaum (Verzweigung baum1 (P name alter g ws) baum2) ((P name1 alter1 g1 ws1):ps)
            | name1 < name && baum1 == Leer = fillBaum (Verzweigung (Verzweigung Leer (P name1 alter1 g1 ws1) Leer) (P name alter g ws) Leer) ps
            | name1 < name = fillBaum (Verzweigung (fillBaum baum1 ((P name1 alter1 g1 ws1):[])) (P name alter g ws) baum2) ps
            | name1 > name && baum2 == Leer = fillBaum (Verzweigung Leer (P name alter g ws) (Verzweigung Leer (P name1 alter1 g1 ws1) Leer)) ps 
            | name1 > name = fillBaum (Verzweigung baum1 (P name alter g ws) (fillBaum baum2 ((P name1 alter1 g1 ws1):[]))) ps
            | name1 == name && alter1 == alter && g1 == g = fillBaum (Verzweigung baum1 (P name alter g (ws ++ ws1)) baum2) ps
            | otherwise = fillBaum (Verzweigung baum1 (P name alter g ws) baum2) ps

bereinige_Anschriften :: Registerbaum -> Registerbaum
bereinige_Anschriften Leer = Leer
bereinige_Anschriften (Verzweigung baum1 (P n a g wss) baum2) = (Verzweigung (bereinige_Anschriften baum1) (P n a g (rdHelper [] (sort wss))) (bereinige_Anschriften baum2))
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

rolle_rueckwaerts :: Registerbaum -> Melderegister
rolle_rueckwaerts Leer = []
rolle_rueckwaerts (Verzweigung Leer p Leer) = [p]
rolle_rueckwaerts (Verzweigung baum1 p baum2) = (rolle_rueckwaerts baum2) ++ [p] ++ (rolle_rueckwaerts baum1)

melderegister :: Integer -> Melderegister
melderegister 1 = [(P name alter geschlecht ws) | name <- ["n5","n2","n3","n5"], 
												alter <- [1], 
												geschlecht <- [X],
												ws <- [(A gemeinde strasse hausnr) | gemeinde <- ["gma1"],
																				     strasse <- ["stra"],
																				     hausnr <- [1]
																					]:[]
											]

melderegister 2 = [(P name alter geschlecht ws) | name <- ["n1","n2","n3"], 
												alter <- [1..2], 
												geschlecht <- [X,W,M],
												ws <- [(A gemeinde strasse hausnr) | gemeinde <- ["gma2","gma1"],
																				     strasse <- ["stra","strb"],
																				     hausnr <- [1,3]
																					]:[]
											]

