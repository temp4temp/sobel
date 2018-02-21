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

import Vision.Image.Filter.Internal hiding (gaussianBlur)

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
pro =   apply (forsim ( ix2 5 5))

type Acc = ((GreyPixel,GreyPixel), (GreyPixel,GreyPixel))

iacc :: Acc
iacc = ((0,0),(0,0))

forsim :: Size -> Filter GreyPixel (Kernel GreyPixel Acc Acc) Acc (FilterFold Acc) Acc GreyPixel
forsim size = 
  Filter size KernelAnchorCenter (Kernel kernel) (\pt sc -> iacc) (FilterFold (const iacc)) post BorderReplicate
  where
    kernel init (Z :. x :. y) souce acc = quadr souce x y init 
    post _ _ _ acc@((a,b), (c,d)) = a

quadr :: (Integral r) => r -> Int -> Int -> ((r, r), (r, r)) -> ((r, r), (r, r))
quadr v a b acc@((r1, r2), (r3, r4)) | a == 0 || b == 0 || v == 0  =  acc
                                     | a < 0 && b < 0 = ((r1 + 1, r2), (r3, r4))
                                     | a < 0 && b > 0 = ((r1, r2 + 1), (r3, r4))
                                     | a > 0 && b < 0 = ((r1, r2), (r3 + 1, r4))
                                     | a > 0 && b > 0 = ((r1, r2), (r3, r4 + 1))

main :: IO ()
main = do
  putStrLn "hello world"
  (Right (g:gs)) <- runExceptT $ readMNIST 10 10 "../../data/train1000.csv"
  savePngImage "one.png" $ ImageY8 $ toJuicyGrey g
  print ""
