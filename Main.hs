{-# LANGUAGE PatternGuards #-}

module Main where

import Debug.Trace
import Codec.Picture ( DynamicImage
                     , Image(..)
                     , PixelRGB8
                     , readImage
                     , writePng
                     )
import Control.Monad ( when )
import Data.List.Split ( splitOn )
import Sdh.Image ( toRgb
                 , transform2d
                 , transform
                 , bendEdges
                 , rangeMap
                 , inverse
                 , cubic
                 )
import System.Environment ( getArgs )

help :: String
help = unlines [ "Usage: imgtrans <infile> <command [args]>... <outfile>"
               , "Commands:"
               , "  -stretch <x-factor>:<y-factor>"
               , "  -stretch square"
               , "  -bend <ax>:<bx>:<ay>:<by>"
               , "  -bend square"
               , "  -fade ...?"
               ]

class ToDouble a where
  toDouble :: a -> Double

instance ToDouble Double where
  toDouble x = x

instance ToDouble Int where
  toDouble = fromIntegral

infixl 7 ./., .*.
(.*.), (./.) :: (ToDouble a, ToDouble b) => a -> b -> Double
x .*. y = (toDouble x) * (toDouble y)
x ./. y = (toDouble x) / (toDouble y)

infixl 6 .+., .-.
(.+.), (.-.) :: (ToDouble a, ToDouble b) => a -> b -> Double
x .+. y = (toDouble x) + (toDouble y)
x .-. y = (toDouble x) - (toDouble y)

-- type ImgTrans a = State (Image PixelRGB8, [String]) a

type Img = Image PixelRGB8

commands :: [(String, String -> Img -> Either String Img)]
commands = [ ("stretch", stretchImage)
           , ("bend", bendImage)
           -- , ("fade", fadeImage)
           ]

stretchImage :: String -> Img -> Either String Img
stretchImage arg img 
  | arg == "square" = let (x, y, x', y') = squareImage img
                      in Right $ stretch' (x' ./. x) (y' ./. y)
  | [Just xs, Just ys] <- map maybeRead $ splitOn ":" arg 
                    = Right $ stretch' xs ys
  | otherwise = Left $ "Could not parse argument to stretch: " ++ arg
  where stretch' :: Double -> Double -> Img
        stretch' xs ys = transform2d img cubic transf xl yl
          where transf x y = (x ./. xs, y ./. ys)
                xl = round $ imageWidth img .*. xs
                yl = round $ imageHeight img .*. ys
        
bendImage :: String -> Img -> Either String Img
bendImage arg img
  | arg == "square" = let (x, y, x', y') = squareImage img
                      in Right $ bend'' x y x' y'
  | [Just ax, Just bx, Just ay, Just by] <- map maybeRead $ splitOn ":" arg
                    = Right $ bend' ax bx ay by
  | otherwise = Left $ "Could not parse argument to bend: " ++ arg
  where bend' :: Int -> Int -> Int -> Int -> Img
        bend' ax bx ay by = transform img cubic xs ys
          where xt = if ax < bx 
                     then inverse $ be ax bx x
                     else be bx ax x'
                yt = if ay < by
                     then inverse $ be ay by y
                     else be by ay y'
                xs = rangeMap xt x'
                ys = rangeMap yt y'
                x = imageWidth img
                y = imageHeight img
                x' = x + 2 * (bx - ax)
                y' = y + 2 * (by - ay)
                be a b z = bendEdges (toDouble a) (toDouble b) (toDouble z)
        bend'' :: Int -> Int -> Int -> Int -> Img
        bend'' x y x' y' = bend' ax bx ay by
          where (ax, bx) = "(ax, bx)" =$ pair $ (x' - x) `div` 2
                (ay, by) = "(ay, by)" =$ pair $ (y' - y) `div` 2
                pair :: Int -> (Int, Int)
                pair dz | dz < 0 = (-2 * dz, -dz)
                        | otherwise = (dz, 2 * dz)

squareImage :: Img -> (Int, Int, Int, Int)
squareImage img = (x, y, x', y') 
  where img' = tr "squareImage" img
        x = "x" =$ imageWidth img'
        y = "y" =$ imageHeight img'
        diff = (x - y) ./. (2 :: Int)
        dx = -diff * (x ./. (x+y))
        dy = diff * (y ./. (x+y))
        x' = "x'" =$ round $ x .+. 2 * dx
        y' = "y'" =$ round $ y .+. 2 * dy

main :: IO ()
main = do args <- getArgs
          when (null args) $ putStrLn help >> fail ""
          imgRead <- readImage $ head args
          img <- case imgRead of
            Left message -> putStrLn ("Could not decode image: " ++ message) >> fail ""
            Right img' -> return img'
          let result = process (tail args) $ toRgb img
          case result of
            Right (outFile, img') -> writePng outFile $ img'
            Left message -> putStrLn message >> fail ""

process :: [String] -> Img -> Either String (String, Img)
process (filename:[]) img = Right (filename, img)
process (('-':cmd):arg:rest) img 
  | Just cmd' <- lookup cmd commands = cmd' arg img >>= process rest
process _ _ = Left help

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
        -- bend a b n z = z * (n+2*(b-a))/n
        bend = bendEdges

infixr 0 =$
(=$) :: Show a => String -> a -> a
name =$ val = trace (name ++ " = " ++ show val) val

tr :: String -> a -> a
tr = trace

maybeRead :: Read a => String -> Maybe a
maybeRead s | [(x, "")] <- reads s = Just x
            | otherwise = Nothing
