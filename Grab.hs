{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Grab where

import BasePrelude hiding (try, takeWhile)
import Control.Lens
import Data.Attoparsec.Text.Lazy
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Graphics.Rendering.Cairo hiding (x, y, width)
import Network.Wreq
import System.Directory as D
import System.FilePath

data Color = Black
           | White

data Piece = King
           | Queen
           | Rook
           | Bishop
           | Knight
           | Pawn
           deriving (Enum, Bounded)

wikipediaChar :: Piece -> [Char]
wikipediaChar King = "k"
wikipediaChar Queen = "q"
wikipediaChar Rook = "r"
wikipediaChar Bishop = "b"
wikipediaChar Knight = "n"
wikipediaChar Pawn = "p"

url :: Color -> Piece -> [Char]
url color piece = "http://en.wikipedia.org/wiki/File:Chess_" <> (wikipediaChar piece) <> (case color of Black -> "d"; White -> "l") <> "t45.svg"

pieces :: [Piece]
pieces = [minBound..]

download :: FilePath -> Color -> Piece -> IO ()
download path color piece = do
  putStrLn ("here we go"  <> path)
  r <- get (url color piece)
  s <- get ("http:" <> (TL.unpack $ parse' (r ^. responseBody)))
  BSL.writeFile path (s ^. responseBody)
  return ()
  where
    urlParser = anchorParser <|> (anyChar *> urlParser)
    anchorParser = string "<a href=\"" *> takeWhile (/= '"') <* anchorEnd
    anchorEnd = takeWhile (/= '>') *> string ">Original file</a>"
    parse' s = TL.fromStrict (fromJust (maybeResult (parse urlParser (TLE.decodeUtf8 s))))

prep :: IO ()
prep = do
  D.createDirectoryIfMissing True "/tmp/pieces"
  forM_ pieces $ \piece -> do
    let path color = ("/tmp/pieces/" <> wikipediaChar piece <> "-" <> (case color of Black -> "b"; White -> "w") <> ".svg")
    download (path White) White piece
    download (path Black) Black piece

checker :: Double -> Int -> Render ()
checker size i =
  case (row + col) `mod` 2 of
    0 -> do
      rectangle x y width width
      setSourceRGBA (147 / 255) (69 / 255) (20/ 255) 1
      fill
    1 ->
      return ()
  where
    width = size / 8
    x = (fromIntegral col) * width
    y = (fromIntegral row) * width
    row = i `div` 8
    col = i `mod` 8

board :: IO ()
board =
  withImageSurface FormatRGB24 isize isize $ \surface -> do
    renderWith surface $ do
      rectangle 0 0 size size
      setSourceRGBA (231 / 255) (171 / 255) (87 / 255) 1
      fill
      forM_ [0..63] (checker size)
    surfaceWriteToPNG surface "/tmp/neptune.png"
  where
    size = 500 :: Double
    isize = 500 :: Int
