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
schreibe (Nicht a) = "(" ++ "neg" ++ " " ++ schreibe a ++ " " ++ ")"
schreibe (Und a1 a2) = "(" ++ schreibe a1 ++ " " ++ "und" ++ " " ++ schreibe a2 ++ ")"
schreibe (Oder a1 a2) = "(" ++ schreibe a1 ++ " " ++ "oder" ++ " " ++ schreibe a2 ++ ")" 
schreibe (Impl a1 a2) = "(" ++ schreibe a1 ++ " " ++ "=>" ++ " " ++ schreibe a2 ++ ")" 
schreibe (Esgibt v a) = "(" ++ "EG" ++ " " ++  schreibe (V v) ++ "." ++ " " ++ schreibe a ++")"
schreibe (Fueralle v a) = "(" ++ "FA" ++ " " ++  schreibe (V v) ++ "." ++ " " ++ schreibe a ++")"

-- Exercise 6.2
import Data.List (sort, sortBy, nub)
import Data.Ord (comparing)

type Nat1 = Int
type Wahrheitswert = Bool
type Name = String
type Alter = Nat1
data Geschlecht = M | W | X deriving Show
type Gemeinde = String
type Strasse = String
type Hausnr = Nat1
data Person = P Name Alter Geschlecht Wohnsitze deriving Show
data Anschrift = A Gemeinde Strasse Hausnr deriving Show
type Wohnsitze = [Anschrift]
type Von_Anschrift = Anschrift
type Nach_Anschrift = Anschrift
type Melderegister = [Person]

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


einwohner :: Melderegister -> Gemeinde -> [(Name,Geschlecht,Alter)]
einwohner [] gm = []
einwohner ps "" = []
einwohner ((P name alter geschlecht []):mrs) gm = einwohner mrs gm
einwohner ((P name alter geschlecht ((A gemeinde strasse hausnr):wss)):mrs) gm 
	| gm == gemeinde = sortBy sortL ((name, geschlecht, alter): (einwohner mrs gm))
	| gm /= gemeinde && wss /= [] = einwohner ((P name alter geschlecht wss):mrs) gm 
	| otherwise = []
	where
		sortL :: (Name,Geschlecht,Alter) -> (Name,Geschlecht,Alter) -> Ordering
		sortL (name1, geschlecht1, alter1) (name2, geschlecht2, alter2)
			| name1 /= name2 = compare name1 name2
			| name1 == name2 && geschlecht1 /= geschlecht2 = compare geschlecht1 geschlecht2
			| otherwise = compare alter1 alter2


durchschnittsalter_mit_Geschlecht_in :: Melderegister -> Geschlecht -> Gemeinde -> Alter
durchschnittsalter_mit_Geschlecht_in [] geschlecht gem = 99999
durchschnittsalter_mit_Geschlecht_in mrs geschlecht "" = 99999
durchschnittsalter_mit_Geschlecht_in mrs geschlecht gem 
	| sumAnzahl (einwohner mrs gem) geschlecht == 0 = 99999
	| otherwise = quot (sumAlter (einwohner mrs gem) geschlecht) (sumAnzahl (einwohner mrs gem) geschlecht)
	where 
		sumAlter :: [(Name, Geschlecht, Alter)] -> Geschlecht -> Alter
		sumAlter [] gs = 0
		sumAlter ((name, geschlecht, alter):seq) gs 
			| gs == geschlecht = alter + sumAlter seq gs
			| otherwise = sumAlter seq gs


		sumAnzahl :: [(Name, Geschlecht, Alter)] -> Geschlecht -> Alter
		sumAnzahl [] gs = 0
		sumAnzahl ((name, geschlecht, alter):seq) gs 
			| gs == geschlecht = 1 + sumAnzahl seq gs
			| otherwise = sumAnzahl seq gs


ist_wohnhaft :: Melderegister -> Name -> Gemeinde -> Wahrheitswert
ist_wohnhaft [] name gemeinde = False
ist_wohnhaft ((P name1 alter geschlecht ((A gemeinde1 strasse hausnr):wss)):mrs) name2 gemeinde2 
	| name1 == name2 && gemeinde1 == gemeinde2 = True
	| name1 /= name2 || wss == [] = ist_wohnhaft mrs name2 gemeinde2
	| name1 == name2 = ist_wohnhaft ((P name1 alter geschlecht wss):mrs) name2 gemeinde2
	| otherwise = False

haben_ausschliesslich_als_Wohnsitz :: Melderegister -> Anschrift -> [Person]
haben_ausschliesslich_als_Wohnsitz [] an = []
haben_ausschliesslich_als_Wohnsitz ((P name1 alter geschlecht (an1:wss)):mrs) an2 
	| an1 == an2 && wss == [] = (P name1 alter geschlecht []) : (haben_ausschliesslich_als_Wohnsitz mrs an2)
	| an1 == an2 = haben_ausschliesslich_als_Wohnsitz ((P name1 alter geschlecht wss):mrs) an2 
	| an1 /= an2 = haben_ausschliesslich_als_Wohnsitz mrs an2
	| otherwise = []

ummelden :: Melderegister -> Von_Anschrift -> Nach_Anschrift -> Melderegister
ummelden [] vonAns nachAns = []
ummelden ((P name1 alter geschlecht wss):mrs) vonAns nachAns = (P name1 alter geschlecht (wssRpl wss vonAns nachAns)) : (ummelden mrs vonAns nachAns)
	where 
		wssRpl :: Wohnsitze -> Von_Anschrift -> Nach_Anschrift -> Wohnsitze
		wssRpl [] vonAns nachAns = []
		wssRpl (ans:wss) vonAns nachAns 
			| ans == vonAns = nachAns : (wssRpl wss vonAns nachAns)
			| otherwise = ans : (wssRpl wss vonAns nachAns)


bereinige_Melderegister :: Melderegister -> Melderegister
bereinige_Melderegister [] = []
bereinige_Melderegister mrs = rdHelper [] mrs
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

melderegister :: Integer -> Melderegister
melderegister 1 = [(P name alter geschlecht ws) | name <- ["n1","n2","n3"], 
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
												ws <- [(A gemeinde strasse hausnr) | gemeinde <- ["gma2"],
																				     strasse <- ["stra"],
																				     hausnr <- [1]
																					]:[]
											]

