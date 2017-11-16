import Data.List (sort)
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

instance Eq Person where
	(P name1 alter1 geschlecht1 ws1) == (P name2 alter2 geschlecht2 ws2) = (name1 == name2) && 
																		   (alter1 == alter2) && 
																		   (geschlecht1 == geschlecht2)

instance Ord Person where
	compare (P name1 alter1 geschlecht1 ws1) (P name2 alter2 geschlecht2 ws2)
		| name1 /= name2 = compare name1 name2
		| name1 == name2 && geschlecht1 /= geschlecht2 = compare geschlecht1 geschlecht2
		| otherwise = compare alter1 alter2


einwohner :: Melderegister -> Gemeinde -> [(Name,Geschlecht,Alter)]
einwohner [] gm = []
einwohner ((P name alter geschlecht []):mrs) gm = einwohner mrs gm
einwohner (P name alter geschlecht ((A gemeinde strasse hausnr):wss):mrs) gm 
	| gm == gemeinde = (name, geschlecht, alter): (einwohner ((P name alter geschlecht wss): (sortL mrs)) gm)
	| otherwise = []

sortL :: [Person] -> [Person]
sortL ps = sort ps

melderegister :: Integer -> Melderegister
melderegister 1 = [(P name alter geschlecht ws) | name <- ["n1","n2","n3"], 
												alter <- [1..2], 
												geschlecht <- [X,W,M],
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
durchschnittsalter_mit_Geschlecht_in :: Melderegister -> Geschlecht -> Gemeinde -> Alter
ist_wohnhaft :: Melderegister -> Name -> Gemeinde -> Wahrheitswert
haben_ausschliesslich_als_Wohnsitz :: Melderegister -> Anschrift -> [Person]
ummelden :: Melderegister -> Von_Anschrift -> Nach_Anschrift -> Melderegister
bereinige_Melderegister :: Melderegister -> Melderegister-}