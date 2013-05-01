module Main where

import Debug.Trace
import Codec.Picture ( DynamicImage
                     , Image(..)
                     , PixelRGB8
                     , readImage
                     , writePng
                     )
import Sdh.Image ( toRgb
                 , transform2d
                 , transform
                 , bendEdges
                 , rangeMap
                 , inverse
                 , cubic
                 )
import System.Environment ( getArgs )
                       
main :: IO ()
main = do args <- getArgs
          if length args /= 2
            then do putStrLn "Usage: imgtrans <file> <xscale> <yscale> <out>"
                    fail "error"
            else return ()
          let file = head args
              -- xs = read $ args !! 1 :: Double
              -- ys = read $ args !! 2 :: Double
              out = args !! 1
          Right img <- readImage file
          -- writePng out $ stretch (toRgb img) xs ys
          writePng out $ stretch2 (toRgb img)

stretch :: Image PixelRGB8 -> Double -> Double -> Image PixelRGB8
stretch img xs ys = transform2d img cubic transf xl yl
  where transf x y = (fromIntegral x / xs, fromIntegral y / ys)
        xl = round $ fromIntegral (imageWidth img) * xs
        yl = round $ fromIntegral (imageHeight img) * ys

stretch2 :: Image PixelRGB8 -> Image PixelRGB8
stretch2 img = transform img cubic xs ys
  where x = s "x" $ fromIntegral $ imageWidth img
        y = s "y" $ fromIntegral $ imageHeight img
        diff = (x - y) / 2
        dx = fromIntegral $ round $ -diff * x / (x+y)
        dy = fromIntegral $ round $ diff * y / (x+y)
        x' = s "x'" $ x + 2 * dx
        y' = s "y'" $ y + 2 * dy
        (ax, bx) = s "(ax, bx)" $ pair dx
        (ay, by) = s "(ay, by)" $ pair dy
        pair :: Double -> (Double, Double)
        pair dz | dz < 0 = (-2 * dz, -dz)
                | otherwise = (dz, 2 * dz)
        xt = if ax < bx 
             then inverse $ bend ax bx x
             else bend bx ax x'
        yt = if ay < by
             then inverse $ bend ay by y
             else bend by ay y'
        xs = s' "xs" $ rangeMap xt (round x')
        ys = s' "ys" $ rangeMap yt (round y')
        s :: Show a => String -> a -> a
        s name val = trace (name ++ " = " ++ show val) val
        s' :: String -> [Double] -> [Double]
        s' name = map snd . s name . zip [0..]
        bend a b n z = z * (n+2*(b-a))/n
