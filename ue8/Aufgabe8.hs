import Data.List

type Wahrheitswert = Bool
data Zeile = Z1 | Z2 | Z3 | Z4 | Z5 | Z6 | Z7 | Z8 | Z9 | Z10 deriving (Eq,Ord,Enum,Show)
data Reihe = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 deriving (Eq,Ord,Enum,Show)
data Haustyp = H00 | H10 | H20 | H30 | H40 | H50 | H60 | H70 | H80 | H90 | H100 deriving (Show,Eq,Ord,Enum)
data Zahl = I | II | III | IV | V | VI | VII | VIII | IX | X deriving (Eq,Ord,Enum,Show)-- Zahlen von 1 bis 10
type Sichtbar = Zahl -- Anzahl sichtbarer Haeuser in einer Blickrichtung.
type Seitenlaenge = Zahl
type Baufeldgroesse = Seitenlaenge -- Zeilen- und Reihenzahl quadratischer Baufelder.
data Zeilen_Blickrichtung = WO | OW deriving Eq -- Blickrichtung von Ost nach West.
data Reihen_Blickrichtung = NS | SN deriving Eq -- Blickrichtung von Sued nach Nord.
type Zeilenauflage = Zeile -> Zeilen_Blickrichtung -> Sichtbar -- total definiert
type Reihenauflage = Reihe -> Reihen_Blickrichtung -> Sichtbar -- total definiert
type Baufeldauflage = (Zeilenauflage,Reihenauflage)
type Baufeld = Zeile -> Reihe -> Haustyp -- total definiert; beschreibt leeres bzw.
type Bauplan = [[Haustyp]] -- max. 10 Zeilen mit je max. 10 Reihen, d.h. max. 10 Listen
data Bauplanung = Bauamtsauflagenskandal -- Kein vorgabenkonformer Bauplan moeglich
	| BP Bauplan deriving Show -- Vorgabenkonformer Bauplan
data Gutachten = Unvollstaendig_bebaut
	| Vollstaedig_nicht_konform_bebaut
	| Vollstaendig_konform_bebaut deriving Show

ist_konforme_bebauung :: Baufeldgroesse -> Baufeld -> Baufeldauflage -> Gutachten
ist_konforme_bebauung bfg bf bfa 
	| isUnfinished (toBauplan bfg bf) = Unvollstaendig_bebaut
	| (conformRows (toBauplan bfg bf) bfa Z1) && (conformColumns (getColumns (toBauplan bfg bf)) bfa R1) = Vollstaendig_konform_bebaut
	| otherwise = Vollstaedig_nicht_konform_bebaut

conformRows :: Bauplan -> Baufeldauflage -> Zeile -> Bool
conformRows (hts:bp) (za, ra) z
	| bp == [] = (checkRow hts OW (zti (za z OW))) && (checkRow hts WO (zti (za z WO)))
	| (checkRow hts OW (zti (za z OW))) && (checkRow hts WO (zti (za z WO))) = conformRows bp (za, ra) (incZ z)
	| otherwise = False

checkRow :: [Haustyp] -> Zeilen_Blickrichtung -> Int -> Bool
checkRow (ht1:ht2:bp) zbr s
	| bp == [] && (ht2 > ht1) = s == 2
	| bp == [] && (ht2 <= ht1) = s == 1
	| zbr == OW = checkRow (reverse (ht1:ht2:bp)) WO s
	| (ht2 > ht1) = checkRow (ht2:bp) zbr (s - 1)
	| (ht2 <= ht1) = checkRow (ht1:bp) zbr s
	| otherwise = False

conformColumns :: Bauplan -> Baufeldauflage -> Reihe -> Bool
conformColumns (hts:bp) (za, ra) z
	| bp == [] = (checkColumn hts NS (zti (ra z NS))) && (checkColumn hts SN (zti (ra z SN)))
	| (checkColumn hts NS (zti (ra z NS))) && (checkColumn hts SN (zti (ra z SN))) = conformColumns bp (za, ra) (incR z)
	| otherwise = False

checkColumn :: [Haustyp] -> Reihen_Blickrichtung -> Int -> Bool
checkColumn (ht1:ht2:bp) zbr s
	| bp == [] && (ht2 > ht1) = s == 2
	| bp == [] && (ht2 <= ht1) = s == 1
	| zbr == SN = checkColumn (reverse (ht1:ht2:bp)) NS s
	| (ht2 > ht1) = checkColumn (ht2:bp) zbr (s - 1)
	| (ht2 <= ht1) = checkColumn (ht1:bp) zbr s
	| otherwise = False

isUnfinished :: Bauplan -> Bool
isUnfinished [] = False
isUnfinished (z1:bp) 
	| H00 `elem` z1 = True
	| otherwise = isUnfinished bp


-- Second part

berechne_bauplan :: Baufeldgroesse -> Baufeld -> Baufeldauflage -> Bauplanung
berechne_bauplan bfg bf (za, ra)
	| (checkSkandal bfg (za, ra) Z1 R1) == False = Bauamtsauflagenskandal
	| otherwise = BP $ toBauplan bfg bf

checkSkandal :: Baufeldgroesse -> Baufeldauflage -> Zeile -> Reihe -> Bool
checkSkandal bfg (za, ra) z r 
	| bfg == (colToZahl r) = (checkZA bfg z za) && (checkRA bfg r ra)
	| (checkZA bfg z za) && (checkRA bfg r ra) = checkSkandal bfg (za, ra) (incZ z) (incR r)
	| otherwise = False


checkZA :: Baufeldgroesse -> Zeile -> Zeilenauflage -> Bool
checkZA bfg z za = ((zti (za z WO)) + (zti (za z OW))) <= ((zti bfg) + 1)


checkRA :: Baufeldgroesse -> Reihe -> Reihenauflage -> Bool
checkRA bfg z za = (zti (za z NS) + zti (za z SN)) <= ((zti bfg) + 1)

-- Helper

toBauplan :: Baufeldgroesse -> Baufeld -> Bauplan
toBauplan bfg bf = [map (bf x) (take (zti bfg) [R1 :: Reihe .. R10 :: Reihe]) | x <- (take (zti bfg) [Z1 :: Zeile .. Z10 :: Zeile])]

getColumns :: Bauplan -> Bauplan
getColumns bp = transpose bp

incZ :: Zeile -> Zeile
incZ Z1 = Z2
incZ Z2 = Z3
incZ Z3 = Z4
incZ Z4 = Z5
incZ Z5 = Z6
incZ Z6 = Z7
incZ Z7 = Z8
incZ Z8 = Z9
incZ Z9 = Z10
incZ Z10 = Z10

incR :: Reihe -> Reihe
incR R1 = R2
incR R2 = R3
incR R3 = R4
incR R4 = R5
incR R5 = R6
incR R6 = R7
incR R7 = R8
incR R8 = R9
incR R9 = R10
incR R10 = R10

zti :: Zahl -> Int
zti I = 1
zti II = 2
zti III = 3
zti IV = 4
zti V = 5
zti VI = 6
zti VII = 7
zti VIII = 8
zti IX = 9
zti X = 10 

itz :: Int -> Zahl
itz 1 = I
itz 2 = II
itz 3 = III
itz 4 = IV
itz 5 = V
itz 6 = VI
itz 7 = VII
itz 8 = VIII
itz 9 = IX
itz 10 = X

colToZahl :: Reihe -> Zahl
colToZahl r = itz $ length $ [R1 :: Reihe .. r]

za :: Zeilenauflage
za Z1 WO = V
za _ WO  = II

bf :: Int -> [Haustyp]
bf 1 = [H10, H20, H50, H40, H50]

testauflage :: Baufeldauflage
testzeilenauflage :: Zeilenauflage
testreihenauflage :: Reihenauflage
testauflage = (testzeilenauflage,testreihenauflage)
testzeilenauflage Z1 WO = IV
testzeilenauflage Z2 WO = II
testzeilenauflage Z3 WO = II
testzeilenauflage Z1 OW = I
testzeilenauflage Z2 OW = II
testzeilenauflage Z3 OW = II
testreihenauflage R1 NS = I
testreihenauflage R1 SN = I
testreihenauflage R2 NS = III
testreihenauflage R2 SN = I
testreihenauflage R3 NS = I
testreihenauflage R3 SN = II

testfeld :: Baufeld
testfeld Z1 R1 = H10
testfeld Z1 R2 = H20
testfeld Z1 R3 = H30
testfeld Z2 R1 = H10
testfeld Z2 R2 = H30
testfeld Z2 R3 = H20
testfeld Z3 R1 = H10
testfeld Z3 R2 = H50
testfeld Z3 R3 = H20
