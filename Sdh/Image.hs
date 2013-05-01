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

-- | A convolution kernel.  Given fractional coordinates (x, y),
-- returns a list of integral coordinates and relative weights
type Kernel = Double -> Double -> [(Int, Int, Double)]

-- | Generates a 2d convolution kernel from a 1d kernel.
genKernel :: (Double -> [(Int, Double)]) -> Kernel
genKernel f x y = [(x', y', cx * cy) | (x', cx) <- f x, (y', cy) <- f y]

-- | Cubic interpolation.  We have the formulas for a full
-- (-2, 2) convolution, but we only use the nearest two pixels
-- in order to remain within gamut (since the next-nearest neighbors
-- have negative coefficients)
cubic :: Kernel
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

-- | Reads a pixel from an image, possibly interpolating.
getPixel :: Kernel           -- ^ Convolution kernel for interpolation
         -> Image PixelRGB8  -- ^ Image to read pixels from
         -> Double           -- ^ x coordinate
         -> Double           -- ^ y coordinate
         -> PixelRGB8
getPixel kernel img x y = average pixels
  where pixels :: [(Double, PixelRGB8)]
        pixels = map (\(x', y', w) -> (w, pix x' y')) weights
        weights :: [(Int, Int, Double)]
        weights = kernel x y
        width = imageWidth img
        height = imageHeight img
        pix x' y' = JP.pixelAt img (clamp 0 width x') (clamp 0 height y')
        clamp a b z | z < a = a
                    | z >= b = b - 1
                    | otherwise = z

-- | Converts a DynamicImage type to a simple RGB8 image.
toRgb :: DynamicImage -> Image PixelRGB8
toRgb (ImageRGB8 img) = img
toRgb (ImageRGBA8 img) = JPT.dropAlphaLayer img
toRgb (ImageY8 img) = JPT.promoteImage img
toRgb (ImageYA8 img) = JPT.promoteImage img
toRgb (ImageYCbCr8 img) = JP.pixelMap JPT.convertPixel img
toRgb (ImageYF img) = JP.pixelMap pixelFtoPixelRGB img
toRgb (ImageRGBF img) = JP.pixelMap pixelRGBFtoPixelRGB img

pixelFtoPixelRGB :: JPT.PixelF -> PixelRGB8
pixelFtoPixelRGB x = JPT.promotePixel $ (round $ x * 255 :: JPT.Pixel8)

pixelRGBFtoPixelRGB :: JPT.PixelRGBF -> PixelRGB8
pixelRGBFtoPixelRGB (JPT.PixelRGBF r g b) = PixelRGB8 (f r) (f g) (f b)
  where f x = round $ x * 255

-- | Applies an arbitrary coordinate transformation.
transform2d :: Image PixelRGB8  -- ^ Source image
            -> Kernel           -- ^ Interpolation kernel
            -> (Int -> Int -> (Double, Double)) 
                -- ^ Coordinate transformation (target-to-source)
            -> Int              -- ^ Width of generated image
            -> Int              -- ^ Height of generated image
            -> Image PixelRGB8
transform2d img kernel func = JP.generateImage generator
  where generator x y = let (x', y') = func x y 
                        in getPixel kernel img x' y'

-- | Applies an arbitrary coordinate transformation.
transform :: Image PixelRGB8  -- ^ Source image
          -> Kernel           -- ^ Interpolation kernel
          -> [Double]         -- ^ X coordinate transform
          -> [Double]         -- ^ Y coordinate transform
          -> Image PixelRGB8
transform img kernel xt yt  = snd $ JP.generateFoldImage generator 
                                          (xt, yt) width height
  where generator s x y = gen s
        gen ([], []) = error "fold called too often"
        gen ([], y:ys) = gen (xt, ys)
        gen (x:xs, y:ys) = ((xs, y:ys), getPixel kernel img x y)
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

inverse :: (Double -> Double) -> Double -> Double
inverse f y = root (\x -> f x - y)
  where root f' = iter y (f' y) (y + 1) (f' $ y + 1)
          where iter x1 y1 _ _ | abs y1 < 1e-7 = x1
                iter x1 y1 x2 y2 = let x0 = x1 - y1 * (x1 - x2) / (y1 - y2)
                                       y0 = f' x0
                                   in iter x0 y0 x1 y1
