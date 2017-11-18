import Data.List (sort, sortBy)
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
{-
bereinige_Melderegister :: Melderegister -> Melderegister-}