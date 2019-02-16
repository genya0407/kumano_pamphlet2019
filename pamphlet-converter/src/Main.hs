{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pandoc
import Text.Pandoc.Definition
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad

-- spanWith (==1) [2,1,2,2,2,1,1] == [[2], [1,2,2,2], [1], [1]]
-- spanWith (==1) [1,2,1,2,2,2,1,1] == [[1,2], [1,2,2,2], [1], [1]]
spanWith :: (a -> Bool) -> [a] -> [[a]]
spanWith cond list = reverse . map reverse $ _spanWith cond list []
  where
    _spanWith :: (a -> Bool) -> [a] -> [[a]] -> [[a]]
    _spanWith _ [] acc = acc
    _spanWith _cond (x:xs) [] = _spanWith _cond xs [[x]]
    _spanWith _cond (x:xs) (curr:acc) = if _cond x
                                        then _spanWith _cond xs ([x]:curr:acc)
                                        else _spanWith _cond xs ((x:curr):acc)

splitChapters :: [Block] -> [[Block]]
splitChapters = spanWith isChapter
  where
    isChapter (Header 1 _ _) = True
    isChapter _ = False

chapterTitle (Header 1 _ contents:_) = T.unpack title
    where
      Right title = runPure $ writePlain def (Pandoc nullMeta [Plain contents])
chapterTitle _ = undefined

readDoc = readLaTeX def
writeDoc = writeHtml5String def

main :: IO ()
main = do
  txt <- T.getContents
  Right pdc@(Pandoc meta ast) <- runIO $ readDoc txt
  let chapters = splitChapters ast
      chapterFileName chapter = "pages/" ++ chapterTitle chapter ++ ".html"
  forM_ chapters $ \chapter -> do
    Right txt <- runIO $ writeDoc (Pandoc meta chapter)
    T.writeFile (chapterFileName chapter) txt
  let chapterAnchor chapter = "<a href='./" <> chapterFileName chapter <> "'>" <> chapterTitle chapter <> "</a>"
  writeFile "index.html" . unlines $ map chapterAnchor chapters
