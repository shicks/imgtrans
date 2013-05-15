{-# LANGUAGE MultiParamTypeClasses, 
             FunctionalDependencies, 
             TypeSynonymInstances, 
             FlexibleInstances, 
             UndecidableInstances #-}

module Sdh.Image where

import Codec.Picture ( DynamicImage(..)
                     , Image(..)
                     , Pixel
                     , PixelRGB8(..)
                     )
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JPT
import Control.Arrow ( (***) 
                     )
import Data.Colour ( Colour(..) )
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as SRGB
import qualified Data.MemoCombinators as Memo

-- | A color on which we can perform a weighted average.
-- This is where we do all the connection between the
-- different libraries.
class Pixel p => Averageable p where
  -- | Performs a weighted average.
  average :: [(Double, p)]  -- ^ Pairs of (weight, color)
          -> p

instance Averageable PixelRGB8 where
  average [] = error "Cannot average empty list" 
  average ((_, p):[]) = p
  average xs@(x:rest) = toPix $ C.affineCombo rest' $ fromPix $ snd x
    where tot = sum $ map fst xs
          rest' = map ((/tot) *** fromPix) $ rest
          toPix :: Colour Double -> PixelRGB8
          toPix c = let SRGB.RGB r g b = SRGB.toSRGBBounded c 
                    in PixelRGB8 r g b
          fromPix :: PixelRGB8 -> Colour Double
          fromPix (PixelRGB8 r g b) = SRGB.sRGB24 r g b

-- | A generalized image.  @HasPixels p i@ should be read as @i = Image p@.
class HasPixels p i | i -> p where
  -- | Returns the pixel at the given coordinates.
  pixelAt :: i -> Int -> Int -> p
  -- | Returns the width of the image.
  pixelWidth :: i -> Int
  -- | Returns the height of the image.
  pixelHeight :: i -> Int

instance Pixel p => HasPixels p (Image p) where
  pixelAt = JP.pixelAt
  pixelWidth = imageWidth
  pixelHeight = imageHeight

-- | A memoized function that can be used for recursive transforms.
data LazyImage p = LazyImage Int Int (Int -> Int -> p)

instance HasPixels p (LazyImage p) where
  pixelAt (LazyImage _ _ f) = f
  pixelWidth (LazyImage w _ _) = w
  pixelHeight (LazyImage _ h _) = h

-- | Generates a real image from a lazy image.
generateImage :: (Pixel p, HasPixels p i) => i -> Image p
generateImage i = JP.generateImage (pixelAt i) (pixelWidth i) (pixelHeight i)

-- | Constructs a 'LazyImage' from a strict 'Image'
lazyImage :: HasPixels p i => i -> LazyImage p
lazyImage i = LazyImage (pixelWidth i) (pixelHeight i) (pixelAt i)

memoize :: Pixel p => LazyImage p -> LazyImage p
memoize (LazyImage width height func) = LazyImage width height $ 
                                        curry $ memo $ uncurry func
  where memo = Memo.pair Memo.integral Memo.integral

-- | A type that is able to get pixels.
class Pixel p => ToPixel a p k | k -> a where
  -- | Reads a pixel from an image, possibly interpolating.
  getPixel :: HasPixels p i
           => k         -- ^ Kernel for interpolation
           -> i         -- ^ Image to read pixels from
           -> a         -- ^ Generalized coordinate(s)
           -> p

instance Averageable p => ToPixel a p (Kernel a) where
  getPixel (Kernel kernel) img coords = average pixels
    where pixels = map (\(x', y', w) -> (w, pix x' y')) weights
          weights :: [(Int, Int, Double)]
          weights = kernel coords
          width = pixelWidth img
          height = pixelHeight img
          pix x' y' = pixelAt img (clamp 0 width x') (clamp 0 height y')
          clamp a b z | z < a = a
                      | z >= b = b - 1
                      | otherwise = z

-- | A generalized pair.  This is needed because we can't declare 
-- @ToPixel (a, b) p k@ so we instead write @(Pair a b t, ToPixel t p k)@.
-- @Pair a b p@ can essentially be read as @p = (a, b)@.
class Pair a b p | p -> a b where 
  pFst :: p -> a
  pSnd :: p -> b
  mkPair :: a -> b -> p

instance Pair a b (a, b) where
  pFst = fst
  pSnd = snd
  mkPair = (,)

-- | A convolution kernel.  Given fractional coordinates (x, y),
-- returns a list of integral coordinates and relative weights
newtype Kernel a = Kernel (a -> [(Int, Int, Double)])

-- | Generates a 2d convolution kernel from a 1d kernel.
genKernel :: (a -> [(Int, Double)]) -> Kernel (a, a)
genKernel f = Kernel kernel 
  where kernel (x, y) = [(x', y', cx * cy) | (x', cx) <- f x, (y', cy) <- f y]

-- | Cubic interpolation.  We have the formulas for a full
-- (-2, 2) convolution, but we only use the nearest two pixels
-- in order to remain within gamut (since the next-nearest neighbors
-- have negative coefficients)
cubic :: Kernel (Double, Double)
cubic = genKernel f
  where f x | x == fromIntegral rx = [(rx, 1)]
            | otherwise = [(fx - 1, poly $ x - fromIntegral fx + 1),
                           (fx, poly $ x - fromIntegral fx),
                           (cx, poly $ fromIntegral cx - x),
                           (cx + 1, poly $ fromIntegral cx - x + 1)]
          where rx = round x
                fx = floor x
                cx = ceiling x
        poly x | x < 0 = poly $ -x
               | x < 1 = 1.5 * x3 - 2.5 * x2 + 1
               | x < 2 = -0.5 * x3 + 2.5 * x2 - 4 * x + 2
               | otherwise = 0
          where x2 = x * x
                x3 = x2 * x

-- | Gaussian blur.  This is a convolution kernel where the "pixel"
-- to return is not just a position but also the blur radius along
-- the given direction, i.e. (Int, Int).
gaussian :: Kernel ((Int, Int), (Int, Int))
gaussian = genKernel f
  where f (x, dx) = map (\(dx, z) -> (x + dx, z)) $ gaussian_ dx

-- | Memoized Gaussian distribution, so that we're not recalculating this list
-- for every pixel.
gaussian_ :: Int -> [(Int, Double)]
gaussian_ = Memo.integral $ g . fromIntegral
  where g :: Double -> [(Int, Double)]
        g s = let vals = map (\x -> exp (-x^2 / (2*s^2))) [1..s]
              in (0, 1) : zip [1..] vals ++ zip [-1, -2..] vals

mix :: Averageable p => p -> Double -> p -> p
mix c' w' c = average [(w', c'), (1-w', c)]

{-
-- | Defines an image transformation in terms of already-transformed
-- pixels.  This is tricky, since we clearly need to provide for a base
-- case as well.
recursiveTransform :: (ToPixel a p k)
                   => Image p
                   -> k
                   -> (Int -> Int -> (a, Int))  -- ^ Transformation function,
                   -- which now returns a number indicating which iteration
                   -- the pixel should be taken from.
                   -> Int 
                   -> Int
                   -> Int  -- ^ number of iterations???
                   -> Image p
recursiveTransform = undefined
-}

-- TODO(sdh): It seems like we should be able to make a recursive/iterational
-- kernel, except that getPixel takes possibly a different image every time.
-- Instead we'd need to refactor the call to getPixel so that it is curried
-- with the image exactly once.  If this is promised then we should be fine
-- to make a recursive version...  Is there any way to require this with the
-- type system?!?

{-
-- | A generalized pixel result
data Averageable p => PixelResult p = Blur Int Int Int Int
                                    | Cubic Double Double
                                    | Mix Double p (PixelResult p)
                                    | Recursive (PixelResult p)

instance Averageble p => ToPixel

recTrans :: (Averageble p, HasPixels p i) => i -> (Int -> Int -> PixelResult) -> Image p
-}

-- | Converts a 'DynamicImage' type to a simple RGB8 image.
toRgb :: DynamicImage -> Image PixelRGB8
toRgb (ImageRGB8 img) = img
toRgb (ImageRGBA8 img) = JPT.dropAlphaLayer img
toRgb (ImageY8 img) = JPT.promoteImage img
toRgb (ImageYA8 img) = JPT.promoteImage img
toRgb (ImageYCbCr8 img) = JP.pixelMap JPT.convertPixel img
toRgb (ImageYF img) = JP.pixelMap pixelFtoPixelRGB img
  where pixelFtoPixelRGB :: JPT.PixelF -> PixelRGB8
        pixelFtoPixelRGB x = JPT.promotePixel $ (round $ x * 255 :: JPT.Pixel8)
toRgb (ImageRGBF img) = JP.pixelMap pixelRGBFtoPixelRGB img
  where pixelRGBFtoPixelRGB :: JPT.PixelRGBF -> PixelRGB8
        pixelRGBFtoPixelRGB (JPT.PixelRGBF r g b) = PixelRGB8 (f r) (f g) (f b)
        f x = round $ x * 255

-- | Applies an arbitrary coordinate transformation.  The type @a@ is
-- a generalized coordinate type, such as @(Double, Double)@.  The type
-- @p@ is the pixel type, which may need to be averagable depending on
-- the kernel type @k@, which is probably a 'Kernel' @a@.
transform2d :: ToPixel a p k
            => Image p           -- ^ Source image
            -> k                 -- ^ Interpolation kernel
            -> (Int -> Int -> a) -- ^ Coordinate transform (target-to-source)
            -> Int               -- ^ Width of generated image
            -> Int               -- ^ Height of generated image
            -> Image p
transform2d img kernel func = JP.generateImage generator
  where generator x y = getPixel kernel img $ func x y

-- generateImage :: Pixel p => (Int -> Int -> p) -> Int -> Int -> p

recursiveGenerateImage :: Pixel p
                       => (LazyImage p -> Int -> Int -> p)
                       -> Int
                       -> Int
                       -> Image p
recursiveGenerateImage func width height = JP.generateImage func' width height
  where func' = curry $ memoize $ uncurry $ func img   -- :: (Int, Int) -> p
        img = LazyImage width height func'             -- :: LazyImage p
        memoize = Memo.pair Memo.integral Memo.integral

{-
-- some sort of recursive reader monad?
data Transform p a = Transform ((Int -> Int -> Transform p) -> Image p -> a)

instance Pixel p => Monad (Transform p) where
  return x = Transform $ \_ _ -> x

type Transf p = Int -> Int -> p

cubicGet :: (Averageable p, HasPixels p i) => i -> Double -> Double -> p
cubicGet i x y = getPixel cubic i (x, y)
-- Can we do this with a monad?  A single image manipulation, executed per pixel?
-- 
-}

-- | Applies an arbitrary coordinate transformation.  We use a generalized pair,
-- probably for no good reason.  Essentially the type @k@ is 'Kernel' @(a, b)@, but
-- another 'ToPixel' instance may be used instead.
transform :: (Pair a b t, ToPixel t p k)
          => Image p    -- ^ Source image
          -> k          -- ^ Interpolation kernel
          -> [a]        -- ^ X coordinate transform
          -> [b]        -- ^ Y coordinate transform
          -> Image p
transform img kernel xt yt  = snd $ JP.generateFoldImage generator 
                                    (xt, yt) width height
  where generator s x y = gen s
        gen ([], []) = error "fold called too often"
        gen ([], y:ys) = gen (xt, ys)
        gen (x:xs, y:ys) = ((xs, y:ys), getPixel kernel img (mkPair x y))
        width = length xt
        height = length yt

-- | A simple edge-bending transformation
bendEdges :: Double -> Double -> Double -> Double -> Double
bendEdges a b n x | x < 0 = x
                  | x < a = b*x/a + (b-a)/pi * sin (pi*x/a)
                  | x <= n - a = x + b - a
                  | x < n = n + 2*(b-a) - bendEdges a b n (n - x)
                  | otherwise = x + 2*(b-a)

-- | Converts a function into a [Double]
rangeMap :: (Double -> Double) -> Int -> [Double]
rangeMap f x = map (f . fromIntegral) [0..(x-1)]

-- | Inverts a double-valued function.
inverse :: (Double -> Double) -> (Double -> Double)
inverse f y = root (\x -> f x - y)
  where root f' = iter y (f' y) (y + 1) (f' $ y + 1)
          where iter x1 y1 _ _ | abs y1 < 1e-7 = x1
                iter x1 y1 x2 y2 = let x0 = x1 - y1 * (x1 - x2) / (y1 - y2)
                                       y0 = f' x0
                                   in iter x0 y0 x1 y1
