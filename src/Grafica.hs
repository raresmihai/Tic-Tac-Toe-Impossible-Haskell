Module Grafica
(
Desen(..),
deseneaza
)
where

import Graphics.Gloss

data Desen = X | O | Nimic deriving Eq

deseneaza :: desen -> Picture
deseneaza X = color blue (rotate 135 (pictures [rectangleSolid 90 10, rectangleSolid 10 90]))
deseneaza O = color red (thickCircle 35 9)
deseneaza _ = blank


