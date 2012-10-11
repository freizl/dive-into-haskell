> {-# LANGUAGE ExistentialQuantification #-}
> 
> module Main where

1. What looks like

>
> data Obj = forall a. (Show a) => Obj a
> 

2. Expanded example - rendering objects in a raytracer

> type Fragment = Char
> data Ball = Ball
> data Mesh = Mesh
> 
> class Renderable a where
>   hit :: a -> [Fragment] -- returns the "fragments" of all hits with ray
>
> instance Renderable Ball where
>   hit b = "b"
> instance Renderable Mesh where
>   hit m = "m"
>   
> hits :: Renderable a => [a] -> [Fragment]
> hits = concatMap hit
> 
> -- | test1 doesn't compile
> -- test1 = hits [Ball, Mesh]
>
> -- | test2 is not we like
> test2 = hits [Ball, Ball]
> 
> -- | thus comes existential type
> data AnyRenderable = forall a. Renderable a => AnyRenderable a
> instance Renderable AnyRenderable where
>   hit (AnyRenderable a) = hit a
> 
> hits2 :: [AnyRenderable] -> [Fragment]
> hits2 = concatMap hit
>
> test3 = hits2 [AnyRenderable Ball, AnyRenderable Mesh]
> 

3. Dynamic dispatch mechanism of OOP

> class Shape_ a where
>   preimeter :: a -> Double
>   area      :: a -> Double
>
> type Radius = Double
> type Side = Double
> 
> data Circle = Circle Radius deriving (Show)
> data Rectange = Rectange Side Side deriving (Show)
> data Square = Square Side deriving (Show)
> 
> instance Shape_ Circle where
>   preimeter (Circle r) = 2 * pi * r
>   area (Circle r) = pi * r * r
> instance Shape_ Rectange where
>   preimeter (Rectange x y) = 2 * (x + y)
>   area (Rectange x y) = x * y
> instance Shape_ Square where
>   preimeter (Square x) = 4 * x
>   area (Square x) = x * x
> 
> data Shape = forall a. (Show a, Shape_ a) => Shape a
>
> instance Shape_ Shape where
>   preimeter (Shape a ) = preimeter a
>   area (Shape a) = area a
> instance Show Shape where
>   show (Shape a) = show a
>   
> circle :: Radius -> Shape
> circle r = Shape (Circle r)
> rectange :: Side -> Side -> Shape
> rectange x y = Shape $ Rectange x y
> square :: Side -> Shape
> square = Shape . Square
> 
> shapes :: [Shape]
> shapes = [circle 3, rectange 2 3, square 4]
> 

[reference](http://www.haskell.org/haskellwiki/Existential_type)
