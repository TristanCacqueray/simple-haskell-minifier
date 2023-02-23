module TinyBrot where

width, height :: Float
width = 80
height = 24

-- | Screen ratio.
screenRatio :: Float
screenRatio = (-0.5)

-- | Compute z = z*z + c.
type Complex = (Float, Float)
z2 :: Complex -> Complex -> Complex
z2 (cx,cy) (x,y) = (cx + (x*x-y*y), cy + 2*x*y)

mb :: Complex
mb = (-1.4844, 0)

type Time = Float
type Zoom = Float
zoom :: Time -> Zoom
zoom x = 4.18 - 4.179 * (1 - cos (x / 10) ** 8)

type Coord = (Float, Float)
coord :: Zoom -> Complex -> Coord -> Complex
coord z (c,d) (x,y) = (c + z*(x-width/2)/width, (d + z*(y-height/2)/height)*screenRatio)

dot :: Complex -> Float
dot (x,y) = let l = abs(x*y) in if isNaN l then 42 else l

brot :: Int -> Complex -> Float
brot max_iter p = dot . last . take max_iter . iterate (z2 p) $ (0,0)

d :: Zoom -> Coord -> String
d _ (81,_) = "\n"
d z c = if brot 150 (coord z mb c) > 20 then " " else "λ"

x = [0..80]

-- p :: [Coord]
-- p = [(x,y) | y <- [0..h], x <- [0..w+1]]
main :: IO ()
main = pure ()
