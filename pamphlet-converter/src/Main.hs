{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pandoc
import Text.Pandoc.Definition
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Maybe
import Data.Either
import Data.List
import Control.Monad
import System.Environment (getArgs)
import System.FilePath.Posix (joinPath, takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Lucid
import Debug.Trace (traceShowId, traceShow)

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
chapterFileName chapter = joinPath ["chapters/", chapterTitle chapter ++ ".html"]

splitSections :: [Block] -> [[Block]]
splitSections = spanWith isSection
  where
    isSection (Header 2 _ _) = True
    isSection _ = False
sectionTitle (Header 2 _ contents:_) = Just $ T.unpack title
  where
    Right title = runPure $ writePlain def (Pandoc nullMeta [Plain contents])
sectionTitle _ = Nothing
sectionTitle' x = fromJust $ sectionTitle x
sectionFileName chapter section = joinPath ["chapters/", chapterTitle chapter , sectionTitle' section ++ ".html"]

readDoc = readLaTeX def
writeDoc = writeHtml5String def

indexHTML :: [(String, String)] -> T.Text
indexHTML subpages = T.pack . LT.unpack . renderText $
  div_ $
    ul_ $
      forM_ subpages $ \(path, name) ->
        li_ $ a_ [href_ $ T.pack (joinPath ["/", path])] (toHtml name)

main :: IO ()
main = do
  outdir <- fmap (!! 0) getArgs
  let writeFile' fname txt = createDirectoryIfMissing True (takeDirectory path) >> T.writeFile path txt where path = joinPath [outdir, fname]
  txt <- T.getContents
  Right pdc@(Pandoc meta ast) <- runIO $ readDoc txt
  let chapters = splitChapters ast
  forM_ chapters $ \chapter -> do
    putStrLn $ chapterTitle chapter
    if "寮生の声" `isPrefixOf` chapterTitle chapter then do
      let header:sections = splitSections chapter
      Right sectionHeader <- runIO . writeDoc $ Pandoc meta header
      let sectionBodies = indexHTML $ map (\s -> (sectionFileName chapter s, sectionTitle' s)) $ filter (isJust . sectionTitle) sections
      writeFile' (chapterFileName chapter) $ sectionHeader <> sectionBodies
      forM_ sections $ \section -> do
        Right txt <- runIO $ writeDoc (Pandoc meta section)
        writeFile' (sectionFileName chapter section) txt
    else do
      Right txt <- runIO $ writeDoc (Pandoc meta chapter)
      writeFile' (chapterFileName chapter) txt
  writeFile' "index.html" . indexHTML $ map (\c -> (chapterFileName c, chapterTitle c)) chapters
