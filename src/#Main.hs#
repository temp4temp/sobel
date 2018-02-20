module Main where

import Vision.Primitive.Shape
import Vision.Image 

import Vision.Image.Filter

import Vision.Image.JuicyPixels (toJuicyGrey)

import Codec.Picture (savePngImage, DynamicImage(..))

import Vision.Primitive

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as V

import           Control.Monad.Trans.Except



createTest :: Grey
createTest = 
  let s = ix2 28 28
  in fromFunction s ( \ (Z :. x :. y ) -> if x > y then 0 else 255 )

vectoGrey :: V.Vector GreyPixel -> Size -> Grey
vectoGrey v s@(Z :. w :. h )  =
  pro $ fromFunction s ( \ (Z :. x :. y ) -> (v V.! ((x * h ) + y)) )



readMNIST :: Int -> Int -> FilePath -> ExceptT String IO [Grey]
readMNIST d t mnist = ExceptT $ do
  mnistdata <- T.readFile mnist
  return $ traverse (A.parseOnly parseMNIST) $ take t $ drop d (T.lines mnistdata)

parseMNIST :: A.Parser Grey
parseMNIST = do
  A.decimal
  pixels   <- A.many' (A.char ',' >> A.decimal)
  vec    <- pure $ V.fromList $ pixels
  pure $ vectoGrey vec $ ix2 28 28

pro :: Grey -> Grey
pro = gaussianBlur 14 (Nothing :: Maybe Double)

forsim :: (Integral src, Integral res) => Size -> Filter src res res res
forsim size = 
  Filter size KernelAnchorCenter (Kernel kernel) (\pt sc -> ()) (FilterFold (const 0)) post BorderReplicate
  where
    kernel init pt souce acc = acc
    post _ _ _ acc  = acc 


main :: IO ()
main = do
  putStrLn "hello world"
  (Right (g:gs)) <- runExceptT $ readMNIST 10 10 "../../data/train1000.csv"
  savePngImage "one.png" $ ImageY8 $ toJuicyGrey g