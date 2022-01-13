module Set8 where

import Data.Char (intToDigit)

import Mooc.Todo

-- This is the final project for Introduction to Functional
-- Programming, part 1. We'll be developing a sort of functional image
-- library together. This file is made up of explanations (like this)
-- and some prepared code. Some definitions you'll have to fill in
-- yourself, just like in the previous exercise sets.

-- We'll use the JuicyPixels library to generate images. The library
-- exposes the Codec.Picture module that has everything we need.
import Codec.Picture

-- Let's start by defining Colors and Pictures.

-- A Color is just three numbers: the red, green and blue components.
-- We use Ints for convenience even though the valid range is only
-- 0-255.
data Color = Color Int Int Int
  deriving (Show,Eq)

getRed :: Color -> Int
getRed (Color r _ _) = r
getGreen :: Color -> Int
getGreen (Color _ g _) = g
getBlue :: Color -> Int
getBlue (Color _ _ b) = b

-- Here are some colors

black :: Color
black = Color 0 0 0

white :: Color
white = Color 255 255 255

pink :: Color
pink = Color 255 105 180

red :: Color
red = Color 255 0 0

yellow :: Color
yellow = Color 255 240 0

-- A coordinate is two Ints, x and y. In this project, the (0,0)
-- coordinate is in the top left corner of the image. The x coordinate
-- increases to the right, and the y coordinate increases down.

data Coord = Coord Int Int

-- A Picture is a wrapper for a function of type Coord -> Color.
-- The function takes in x and y coordinates and returns a color.

data Picture = Picture (Coord -> Color)

-- Here's a picture that's just a white dot at 10,10
justADot = Picture f
  where f (Coord 10 10) = white
        f _             = black

-- Here's a picture that's just a solid color
solid :: Color -> Picture
solid color = Picture (\coord -> color)

-- Here's a simple picture:
examplePicture1 = Picture f
  where f (Coord x y) | abs (x+y) < 100 = pink    -- top corner is pink
                      | max x y < 200 = white     -- surrounded by a white square
                      | otherwise = black         -- rest of the picture is black

-- In order to find out what our example picture looks like, here's a
-- function that renders a Picture into a png file.
--
-- In addition to the Picture it takes a width and a height.
--
-- The return type is IO (). Check Lecture 8 for a short introduction
-- to IO

render :: Picture -> Int -> Int -> String -> IO ()
render (Picture f) w h name = writePng name (generateImage (\x y -> colorToPixel (f (Coord x y))) w h)
  where colorToPixel :: Color -> PixelRGB8
        colorToPixel (Color r g b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- To see examplePicture1, run this in GHCi:
--
--   render examplePicture1 400 300 "example1.png"
--
-- This should produce an example1.png file.
--
-- Remember: You can get open GHCi with `stack ghci Set8.hs`

-- For testing purposes, let's also define some functions for drawing
-- pictures as lists. It's customary to show colours as hexadecimal
-- strings. This is what colorToHex does.

showHex :: Int -> String
showHex i = [digit (div i 16), digit (mod i 16)]
  where digit x | x>=0 && x<16 = intToDigit x
                | otherwise    = 'X'

colorToHex :: Color -> String
colorToHex (Color r g b) = showHex r ++ showHex g ++ showHex b

getPixel :: Picture -> Int -> Int -> String
getPixel (Picture f) x y = colorToHex (f (Coord x y))
renderList :: Picture -> (Int,Int) -> (Int,Int) -> [[String]]
renderList picture (minx,maxx) (miny,maxy) =
  [[getPixel picture x y | x <- [minx..maxx]] | y <- [miny..maxy]]

-- renderListExample evaluates to
-- [["000000","000000","000000"],
--  ["000000","ffffff","000000"],
--  ["000000","000000","000000"]]
renderListExample = renderList justADot (9,11) (9,11)

------------------------------------------------------------------------------
-- Ex 1: define a picture dotAndLine that has a white dot at (3,4) and
-- a pink line at y=8. Everywhere else, the picture is black.
--
-- Example:
--   renderList dotAndLine (2,4) (3,9) ==>
--     [["000000","000000","000000"],
--      ["000000","ffffff","000000"],
--      ["000000","000000","000000"],
--      ["000000","000000","000000"],
--      ["000000","000000","000000"],
--      ["ff69b4","ff69b4","ff69b4"],
--      ["000000","000000","000000"]]

dotAndLine :: Picture
dotAndLine = Picture f
  where f (Coord 3 4) = white
        f (Coord _ 8) = pink
        f (Coord _ _) = black

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Ex 2: blending colors and images.
--
-- Implement the function blendColor that averages two Colors,
-- component by component.
--
-- Implement the function combine that takes a function and two images
-- and makes a new image by applying the function to the corresponding
-- pixels of the original images. For example,
--
--   combine blendColor p1 p2
--
-- should average two images, pixel by pixel.
--
-- PS. Use rounding down integer division (i.e. the div function) for
-- the average.
--
-- Examples:
--   blendColor (Color 10 100 0) (Color 0 200 40)
--     ==> Color 5 150 20
--   renderList (combine (\c1 c2 -> c1) (solid red) justADot) (9,11) (9,11)
--     ==> [["ff0000","ff0000","ff0000"],
--          ["ff0000","ff0000","ff0000"],
--          ["ff0000","ff0000","ff0000"]]
--   renderList (combine blendColor (solid red) justADot) (9,11) (9,11)
--     ==> [["7f0000","7f0000","7f0000"],
--          ["7f0000","ff7f7f","7f0000"],
--          ["7f0000","7f0000","7f0000"]]

blendColor :: Color -> Color -> Color
blendColor (Color r1 g1 b1) (Color r2 g2 b2) =
  Color (div (r1+r2) 2) (div (g1+g2) 2) (div (b1+b2) 2)

combine :: (Color -> Color -> Color) -> Picture -> Picture -> Picture
--combine xform (Picture p1) (Picture p2) = Picture f                 -- 1st one, works
--  where f (Coord x y) = xform (p1 (Coord x y)) (p2 (Coord x y)) 
--combine xform (Picture p1) (Picture p2) = Picture f                 -- 2nd one
--  where f coord = xform (p1 coord) (p2 coord)

-- we get from 2nd one to here pretty easily, they are equivalent!
combine xform (Picture p1) (Picture p2) =                             -- 3rd one
  Picture (\coord -> xform (p1 coord) (p2 coord))
-- How does it know how to get the coords? Think about the xform between 2 and 3.
-- what if theyre not the same size? Does it automatically make the larger size?

------------------------------------------------------------------------------

-- Let's define blend, we'll use it later
blend :: Picture -> Picture -> Picture
blend = combine blendColor

-- In order to draw some more interesting stuff, let's define the
-- notion of a Shape. A Shape is just a function that takes in
-- coordinates and returns a boolean indicating whether the
-- coordinates belong to the shape.

data Shape = Shape (Coord -> Bool)

-- Here's a utility for testing
contains :: Shape -> Int -> Int -> Bool
contains (Shape f) x y = f (Coord x y)

-- The simplest shape is a dot. Here's a function that returns a dot
-- in a given position.

dot :: Int -> Int -> Shape
dot x y = Shape f
  where f (Coord cx cy) = (x==cx) && (y==cy)

-- Here's the definitions of a circle

circle :: Int -> Int -> Int -> Shape
circle r cx cy = Shape f
  where f (Coord x y) = (x-cx)^2 + (y-cy)^2 < r^2

-- To be able to draw a Shape we need to convert it to a Picture.
-- Here's one way: fill the shape with a given color.

fill :: Color -> Shape -> Picture
fill c (Shape f) = Picture g
  where g coord | f coord = c
                | otherwise = black

-- Here's a picture of a red circle. You can see it by running
--   render exampleCircle 400 300 "circle.png"

exampleCircle :: Picture
exampleCircle = fill red (circle 80 100 200)

------------------------------------------------------------------------------
-- Ex 3: implement a rectangle. The value of `rectangle x0 y0 w h`
-- should be a rectangle with the upper left corner at (x0, y0), a
-- width of w, and a height of h.
--
-- Example:
--  renderList (fill white (rectangle 1 2 4 3)) (0,5) (0,5)
--   ==> [["000000","000000","000000","000000","000000","000000"],
--        ["000000","000000","000000","000000","000000","000000"],
--        ["000000","ffffff","ffffff","ffffff","ffffff","000000"],
--        ["000000","ffffff","ffffff","ffffff","ffffff","000000"],
--        ["000000","ffffff","ffffff","ffffff","ffffff","000000"],
--        ["000000","000000","000000","000000","000000","000000"]]

rectangle :: Int -> Int -> Int -> Int -> Shape
rectangle x0 y0 w h = Shape f
  where f (Coord x y) = x >= x0 && x < (x0 + w) && y >= y0 && y < y0 + h
  
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Ex 4: combining shapes.
--
-- We defined Shape in addition to Picture because some operations are
-- easier to define for Shapes. Implement the union and cut functions.
--
-- Any point that belongs to one of the shapes should
-- belong to the union.
--
-- Cut should remove all points of the second shape from the first
-- shape.

union :: Shape -> Shape -> Shape
-- union (Shape a) (Shape b) = Shape f
--  where f (Coord x y) = a (Coord x y) || b (Coord x y)
-- union (Shape a) (Shape b) = Shape f
--   where f (coord) = a (coord) || b (coord)
union (Shape a) (Shape b) =
  Shape (\coord -> a(coord) || b(coord))

cut :: Shape -> Shape -> Shape
--cut (Shape a) (Shape b) = Shape f
--  where f (Coord x y) = a (Coord x y) && not (b (Coord x y))
cut (Shape a) (Shape b) =
  Shape (\coord ->  a(coord) && not (b(coord)))

------------------------------------------------------------------------------

-- Here's a snowman, built using union from circles and rectangles.
-- See it by running
--   render exampleSnowman 400 300 "snowman.png"

-- !!@bob one shape does render (circle) where this shape does not. TODO exception

exampleSnowman :: Picture
exampleSnowman = fill white snowman
  where snowman = union (cut body mouth) hat
        mouth = rectangle 180 180 40 5
        body = union (circle 50 200 250) (circle 40 200 170)
        hat = union (rectangle 170 130 60 5) (rectangle 180 100 40 30)

------------------------------------------------------------------------------
-- Ex 5: even though we can combine Shapes and convert them to Pictures, we
-- can't easily add something to a Picture. Let's fix that.
--
-- Implement the function paintSolid that takes a color and a shape,
-- and draws them on top of an existing picture.
--
-- Example: renderList (paintSolid pink (dot 10 11) justADot) (9,11) (9,12)
--   ==> [["000000","000000","000000"],
--        ["000000","ffffff","000000"],
--        ["000000","ff69b4","000000"],
--        ["000000","000000","000000"]]


-- remember
-- union (Shape a) (Shape b) =
--  Shape (\coord -> a(coord) || b(coord))
paintSolid :: Color -> Shape -> Picture -> Picture
paintSolid c (Shape s) (Picture b) = Picture g
  where g coord | s coord = c
                | otherwise = b coord

-- picture coord -> picture color at that pixel

------------------------------------------------------------------------------

allWhite :: Picture
allWhite = solid white

-- Here's a colorful version of the snowman example. See it by running:
--   render exampleColorful 400 300 "colorful.png"

exampleColorful :: Picture
exampleColorful = (paintSolid black hat . paintSolid red legs . paintSolid pink body) allWhite
  where legs = circle 50 200 250
        body = circle 40 200 170
        hat = union (rectangle 170 130 60 5) (rectangle 180 100 40 30)

-- How about painting with a pattern instead of a solid color? Here
-- are the definitions of two patterns (Pictures).

stipple :: Color -> Color -> Picture
stipple a b = Picture f
  where f (Coord x y) | even x == even y  = a
                      | otherwise         = b

stripes :: Color -> Color -> Picture
stripes a b = Picture f
  where f (Coord x y) | even y    = a
                      | otherwise = b

-- You can check them out:
--   render (stipple red white) 50 50 "stipple.png"
--   render (stripes pink black) 50 50 "stripes.png"

------------------------------------------------------------------------------
-- Ex 6: implement a function paint that works like paintSolid, except
-- the first argument is a pattern (as a Picture).
--
-- Example:
-- renderList (paint (stripes red white) (rectangle 0 0 2 4) (solid black)) (0,4) (0,4)
--  ==> [["ff0000","ff0000","000000","000000","000000"],
--       ["ffffff","ffffff","000000","000000","000000"],
--       ["ff0000","ff0000","000000","000000","000000"],
--       ["ffffff","ffffff","000000","000000","000000"],
--       ["000000","000000","000000","000000","000000"]]

paint :: Picture -> Shape -> Picture -> Picture
paint (Picture pat) (Shape shape) (Picture base) = Picture g
  where g coord | shape coord = pat coord
                | otherwise = base coord
------------------------------------------------------------------------------

-- Here's a patterned version of the snowman example. See it by running:
--   render examplePatterns 400 300 "patterns.png"

examplePatterns :: Picture
examplePatterns = (paint (solid black) hat . paint (stripes red yellow) legs . paint (stipple pink black) body) allWhite
  where legs = circle 50 200 250
        body = circle 40 200 170
        hat = union (rectangle 170 130 60 5) (rectangle 180 100 40 30)

-- What if we want vertical stripes? What if we want wider stripes?
-- Let's implement zooming and flipping images.

flipCoordXY :: Coord -> Coord
flipCoordXY (Coord x y) = (Coord y x)

-- Flip a picture by switching x and y coordinates
flipXY :: Picture -> Picture
flipXY (Picture f) = Picture (f . flipCoordXY)

zoomCoord :: Int -> Coord -> Coord
zoomCoord z (Coord x y) = Coord (div x z) (div y z)

-- Zoom a picture: scale it up by a factor of z
zoom :: Int -> Picture -> Picture
zoom z (Picture f) = Picture (f . zoomCoord z)

-- Here are some large vertical stripes. See them by running
--   render largeVerticalStripes 400 300 "large-stripes.png"
largeVerticalStripes = zoom 5 (flipXY (stripes red yellow))

-- To support all sorts of image transforms let's use a type class
-- Transform. A Transform is something that you can apply to an image.

class Transform t where
  apply :: t -> Picture -> Picture

-- Here's a simple image for testing transformations
xy :: Picture
xy = Picture f
  where f (Coord x y) = Color (mod x 256) (mod y 256) 0

------------------------------------------------------------------------------
-- Ex 7: implement Transform instances for the Fill, Zoom and Flip types.
--
-- The Fill transform should fill the image completely with the given color.
--
-- The Zoom transform should scale a picture up just like the zoom function above.
--
-- The FlipX transform should flip the image along the vertical axis,
-- i.e. map (10,15) to (-10,15).
--
-- The FlipY transform should flip the image along the horizontal
-- axis, i.e. map (10,15) to (10,-15).
--
-- The FlipXY transform should switch the x and y coordinates, i.e.
-- map (10,15) to (15,10).

data Fill = Fill Color

instance Transform Fill where
  apply (Fill color) (Picture base) = Picture (\_ -> color)
  -- apply (Fill color) (Picture base) = Picture (const color)  
  -- apply (Fill color) _ = Picture (const color)

data Zoom = Zoom Int
  deriving Show

instance Transform Zoom where
  --apply (Zoom z)  base = zoom z base
  apply (Zoom z) = zoom z

data Flip = FlipX | FlipY | FlipXY
  deriving Show

instance Transform Flip where
  -- these are composed with dot like above.
  apply FlipX (Picture f) = Picture (f . (\(Coord x y) -> (Coord (-x) y)))
  apply FlipY (Picture f) = Picture (f . (\(Coord x y) -> (Coord x (-y))))
  --apply FlipY (Picture f) = Picture $ f . (\(Coord x y) -> (Coord x (-y))) -- also works
  -- another form of this is :
  -- apply FlipY (Picture f) = Picture (\(Coord x y) -> f (Coord x (-y)))
  
  -- apply FlipXY (Picture f) = flipXY (Picture f) --works
  apply FlipXY f = flipXY f -- also works
  -- apply FlipXY = flipXY-- but this does not.
  --   since apply is apply :: t -> Picture -> Picture
  --          apply t should be a Picture -> Picture
  --   and flipXY is flipXY :: Picture -> Picture
  -- shouldn't it be ok to say : apply FlipXY = flipXY
  -- It complains that the different definitions for apply Flip have differnt numbers
  --  of arguments
  
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Ex 8: the Chain type represents a combination of two transforms.

-- Implement a Transform instance for Chain.
--
-- When (Chain t1 t2) is applied to an image, t2 is first applied to
-- the image, and then t1.
--
-- Hint: you might need a constraint on the instance

data Chain a b = Chain a b
  deriving Show

instance (Transform a, Transform b) => Transform (Chain a b) where
  --apply (Chain a b) picture = apply a (apply b picture)
  apply (Chain a b) picture = ((apply a) . (apply b)) picture
 
    
------------------------------------------------------------------------------

-- Now we can redefine largeVerticalStripes using the above Transforms.
-- See the picture by running
--   render largeVerticalStripes2 400 300 "large-stripes2.png"
largeVerticalStripes2 :: Picture
largeVerticalStripes2 = apply (Chain (Zoom 5) FlipXY) (stripes red yellow)

-- We can also define a nice checkered pattern by overlaying two stripes.
-- See it by running
--    render checkered 400 30 "checkered.png"
flipBlend :: Picture -> Picture
flipBlend picture = blend picture (apply FlipXY picture)

checkered :: Picture
checkered = flipBlend largeVerticalStripes2

------------------------------------------------------------------------------
-- Ex 9: implement a Transform instance for Blur.
--

data Blur = Blur
  deriving Show

addcolors :: Color -> Color -> Color
addcolors (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)

divcolor :: Color -> Int -> Color
divcolor (Color r g b) i = Color (div r i) (div g i) (div b i)
-- sumcolors :: [Color] -> Color
-- sumcolors [] = black
-- sumcolors (x:xs) = addcolors x (sumcolors xs)

-- this is sumcolors:
sumcolors :: Foldable t => t Color -> Color
sumcolors = foldr addcolors black

fetchcross :: Picture -> Int -> Int -> [Color]
fetchcross (Picture b) x y = [left, right, up, down, center]
  where
    center = b (Coord x y)
    left = b (Coord x (y-1))
    right = b (Coord x (y+1))
    up = b (Coord (x-1) y)
    down = b (Coord (x+1) y)

instance Transform Blur where
  apply Blur (Picture b) = Picture f
    --where f (Coord x y ) = b (Coord x y)
    --where f (Coord x y ) = divcolor (getbcolor (Picture b) x y) 5 -- good
    where f (Coord x y ) = divcolor (sumcolors (fetchcross (Picture b) x y)) 5 -- good

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Ex 10: blur an image multiple times. Implement a Transform instance
-- for BlurMany. The transform BlurMany n should perform Blur n times.
--
-- Example: renderList (apply (BlurMany 2) justADot) (8,12) (8,12)
--   ==> [["000000","000000","0a0a0a","000000","000000"],
--        ["000000","141414","141414","141414","000000"],
--        ["0a0a0a","141414","333333","141414","0a0a0a"],
--        ["000000","141414","141414","141414","000000"],
--        ["000000","000000","0a0a0a","000000","000000"]]

data BlurMany = BlurMany Int
  deriving Show

-- instance Transform BlurMany where
--   apply (BlurMany i) (Picture b)
--     | i == 0 = (Picture b)
--     | otherwise = apply (BlurMany (i-1)) (apply Blur (Picture b))

instance Transform BlurMany where
  apply (BlurMany 0) p = p
  apply (BlurMany i) p = apply (BlurMany (i-1)) (apply Blur p)
  
------------------------------------------------------------------------------

-- Here's a blurred version of our original snowman. See it by running
--   render blurredSnowman 400 300 "blurred.png"

blurredSnowman = apply (BlurMany 2) exampleSnowman

-- for some reason this takes a long time. 
