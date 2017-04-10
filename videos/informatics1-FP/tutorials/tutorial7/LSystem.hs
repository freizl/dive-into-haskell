-- LSystem drawing module
--



module LSystem (
    display,
    Command (..),
    Pen (..), black, white, red, green, blue,
    Distance, Angle,
    triangle, tree
   )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck

infixr 5 :#:



-- Points

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

-- The last two functions are not called min and max
-- because the invariant for min and max states
-- (min x y, max x y) = (x,y) or (y,x).

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors

data Pen = Colour GL.GLfloat GL.GLfloat GL.GLfloat
         | Inkless
           deriving (Eq, Ord, Show)

penToRGB :: Pen -> GL.Color3 GL.GLfloat
penToRGB (Colour r g b)  =  GL.Color3 r g b
penToRGB Inkless  =  error "penToRGB: inkless"

white, black, red, green, blue :: Pen
white = Colour 1.0 1.0 1.0
black = Colour 0.0 0.0 0.0
red   = Colour 1.0 0.0 0.0
green = Colour 0.0 1.0 0.0
blue  = Colour 0.0 0.0 1.0

-- Lines

data Ln = Ln Pen Pnt Pnt
  deriving (Eq,Ord,Show)

-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = penToRGB white



-- Main drawing and window functions

display :: Command -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "Turtle Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Command -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background 
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pen startP endP)  =
    GL.color (penToRGB pen) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3 
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



-- Commands for moving the turtle around
--  Turtles turn counter-clockwise and start facing up

type Angle    = Float
type Distance = Float
type Turtle   = (Pen,Angle,Pnt)

data Command = Go Distance
             | Turn Angle 
             | Sit
             | Command :#: Command
             | Branch Command
             | GrabPen Pen
               deriving (Eq, Ord, Show)


-- Converting commands to GL graphics

execute :: Command -> [Ln]
execute c  =  lines
  where
  (lines, turtle)  =  f c (black, 0, Pnt 0 0)

  f :: Command -> Turtle -> ([Ln], Turtle)
  f (c :#: d) turtle             =  (clines ++ dlines, dturtle)
                                    where
                                    (clines, cturtle) = f c turtle
                                    (dlines, dturtle) = f d cturtle
  f (Branch c) turtle            =  (clines, turtle)
                                    where
                                    (clines, cturtle) = f c turtle
  f (Go dst) (pen,ang,pnt)       =  (if pen == Inkless
                                       then []
                                       else [Ln pen pnt endpnt],
                                     (pen,ang,endpnt))
                                    where
                                    endpnt = pnt + scalar dst * polar ang
  f (Turn delta) (pen,ang,pnt)   =  ([], (pen,ang-delta,pnt))
  f (GrabPen new) (old,ang,pnt)  =  ([], (new,ang,pnt))
  f (Sit) turtle		 =  ([], turtle)


-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pen p q)  =  Ln pen (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pen p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x

polar :: Angle -> Pnt
polar ang  =  Pnt (cos radians) (sin radians)
  where
  radians  =  ang * 2 * pi / 360


-- Sample LSystems

triangle :: Int -> Command
triangle x  =  p :#: f x
  where
  f 0      = Go 10
  f x  = f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
  n        = Turn 90
  p        = Turn (-90)

tree :: Int -> Command
tree x  =  f x
  where
  f 0      = GrabPen red :#: Go 10
  f x  = g x :#: Branch (n :#: f (x-1))
                 :#: Branch (p :#: f (x-1))
                 :#: Branch (g (x-1) :#: f (x-1))
  g 0      = GrabPen blue :#: Go 10
  g x  = g (x-1) :#: g (x-1)
  n        = Turn 45
  p        = Turn (-45)



-- Generators for QuickCheck

instance Arbitrary Pen where
    arbitrary  =  sized pen
        where
          pen n  =  elements [black,red,green,blue,white,Inkless]


instance Arbitrary Command where
    arbitrary  =  sized cmd 
        where
          cmd n  |  n <= 0     =  oneof [liftM (Go . abs) arbitrary,
                                         liftM Turn arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))



