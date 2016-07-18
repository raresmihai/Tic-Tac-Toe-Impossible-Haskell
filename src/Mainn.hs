import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Data.Ord
import qualified Data.Map as Map

type Tabela = Map.Map (Int, Int) Desen

data Desen = X | O | Liber deriving Eq

configuratieInitiala = Map.empty

fundal :: Color
fundal = yellow

fps :: Integer
fps = 60

main :: IO ()
main = play
    (InWindow "X si 0" (320, 420) (100, 100))
    fundal
    fps
    configuratieInitiala
    afiseazaTabela
    proceseazaInput
    pas
	
deseneaza :: Desen -> Picture
deseneaza X = color green (rotate 135 (pictures [rectangleSolid 90 10, rectangleSolid 10 90]))
deseneaza O = color red (thickCircle 35 10)
deseneaza _ = blank

obtineDesen :: Tabela -> (Int,Int) -> Desen
obtineDesen t (x,y) = Map.findWithDefault Liber (x,y) t

afiseazaTabela :: Tabela -> Picture
afiseazaTabela t = 
     pictures $ [ color azure $ translate    0   50  $ rectangleSolid 320   3.5 --tabela
               , color azure $ translate    0 (-50) $ rectangleSolid 320   3.5
               , color azure $ translate   50    0  $ rectangleSolid   3.5 320
               , color azure $ translate (-50)   0  $ rectangleSolid   3.5 320
			   , color azure $ translate    0 (-160) $ rectangleSolid 320   3.5
			   , color azure $ translate    0   160 $ rectangleSolid 320   3.5
			   , color azure $ translate (-158)   0  $ rectangleSolid   3.5 320
			   , color azure $ translate   158   0  $ rectangleSolid   3.5 320
               ] ++
			   [ color rose $ translate 0 185 $ rectangleWire 130 30 --titlu
			   , color blue $ translate (-42) 177 $ scale 0.19 0.19 $ text "X si 0"
			   ] ++
               [ translate --Celulele
                    ((fromIntegral x - 1) * 100)
                    ((fromIntegral y - 1) * 100) $
                    deseneaza $ obtineDesen t (x, y)
               | x <- [0..2]
               , y <- [0..2] ] ++
               [				
               color black $ translate (-130) (-193) $ scale 0.13 0.13 $ text $ aflaSituatie t --status
               ]


jucatorCurent :: Tabela -> Desen
jucatorCurent t | randulLuiX t = X
                | otherwise = O

randulLuiX :: Tabela -> Bool
randulLuiX t = odd (length (celuleLibere t))

transformaCoordonata :: Float -> Int
transformaCoordonata x = (max (-1) (min 1 (floor((x+50)/100)))) + 1

proceseazaInput :: Event -> Tabela -> Tabela
proceseazaInput (EventKey (MouseButton LeftButton) Up _ (x, y)) t =
    if obtineDesen t (transformaCoordonata x, transformaCoordonata y) == Liber && jucatorCurent t == X && x >= (-160) && x<= 160 && y >= (-160) && y<=160
	then Map.insert (transformaCoordonata x, transformaCoordonata y) X t
	else if celuleLibere t == [] then Map.empty
	     else t
proceseazaInput _ t = t

celuleLibere :: Tabela -> [(Int, Int)]
celuleLibere t = case castigator t of Nothing -> [ (x, y)
                                               | x <- [0..2]
                                               , y <- [0..2]
                                               , obtineDesen t (x, y) == Liber]
                                      _       -> []
  
pas :: Float -> Tabela -> Tabela
pas _ t =
  if jucatorCurent t == O && (not . null . celuleLibere $ t)
  then plaseazaO t
  else t

castigator :: Tabela -> Maybe Desen
castigator t = maybeHead . map head . filter (\xs -> head xs /= Liber && elementeEgale xs) .
               map (map $ obtineDesen t) $ liniiCastigatoare

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _      = Nothing

elementeEgale :: Eq a => [a] -> Bool
elementeEgale []     = True
elementeEgale (x:xs) = all (==x) xs

liniiCastigatoare :: [[(Int,Int)]]
liniiCastigatoare = [[(0,0),(1,0),(2,0)],[(0,0),(0,1),(0,2)],[(0,0),(1,1),(2,2)],[(0,1),(1,1),(2,1)],[(0,2),(1,2),(2,2)],[(0,2),(1,1),(2,0)],[(1,0),(1,1),(1,2)],[(2,0),(2,1),(2,2)]]
		
aflaSituatie :: Tabela -> String
aflaSituatie t | (length $ celuleLibere t) > 0 = "  Jocul este in desfasurare"
               | castigator t == Just O = "Ati pierdut! Click pentru resetare"
               | castigator t == Just X = "Ati castigat! Click pentru resetare"			   
               | otherwise = " Remiza! Click pentru resetare"	
			   
plaseazaO :: Tabela -> Tabela
plaseazaO t = fst . maximumBy (comparing snd) .
            map (\j -> (j, bkt j False)) $
			map (\k -> Map.insert k O t) (celuleLibere t)

bkt :: Tabela -> Bool -> Int
bkt tabela randulLuiO
  | null $ celuleLibere tabela = scor tabela
  | otherwise =
       let mutari = map
	            (\k -> bkt
				    (Map.insert k (jucatorCurent tabela) tabela)
					(not randulLuiO) )
				(celuleLibere tabela)
		in if randulLuiO then maximum mutari else minimum mutari

scor :: Tabela -> Int
scor t = case castigator t of Just X -> -1
                              Just O -> 1
                              _      -> 0							  