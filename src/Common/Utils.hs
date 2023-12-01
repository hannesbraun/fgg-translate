{-# LANGUAGE OverloadedStrings #-}

module Common.Utils where

import Control.Exception
import Control.Monad
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files

showText :: Show a => a -> T.Text
showText = T.pack . show

tryGetLine :: Handle -> IO (Maybe T.Text)
tryGetLine h = do
  input <- try (T.hGetLine h)
  case input of
    Left e ->
      if isEOFError e
        then return Nothing
        else ioError e
    Right inpStr ->
      return (Just inpStr)

stripPrefix :: T.Text -> T.Text -> Maybe T.Text
stripPrefix prefix t =
  if prefix `T.isPrefixOf` t
    then Just $ T.drop (T.length prefix) t
    else Nothing

prettyToText :: Pretty a => T.Text -> a -> T.Text
prettyToText header x =
  let code = header <> renderStrict (layoutPretty opts (pretty x))
      cleanCode =
        T.unlines $ removeNewlines $ map T.stripEnd (T.lines code)
   in cleanCode
 where
  removeNewlines [] = []
  removeNewlines ("" : rest) =
    "" : removeNewlines (dropWhile T.null rest)
  removeNewlines (x : xs) = x : removeNewlines xs
  opts =
    defaultLayoutOptions
      { layoutPageWidth = AvailablePerLine 120 1.0
      }

-- | Traverse from 'top' directory and return all the files found.
traverseDir :: FilePath -> IO [FilePath]
traverseDir top = do
  ds <- getDirectoryContents top
  paths <- forM (filter (\x -> not (x `elem` [".", ".."])) ds) $ \d -> do
    let path = top </> d
    s <- getFileStatus path
    if isDirectory s
      then traverseDir path
      else return [path]
  return (concat paths)

mapRightM :: Monad m => (a -> m (Either b c)) -> [a] -> m [c]
mapRightM f l = rights <$> mapM f l

outputTrace :: [T.Text] -> IO ()
outputTrace trace =
  forM_ trace $ \t -> putStrLn (T.unpack (prefix <> indent (T.length prefix) t))
 where
  prefix = "[TRACE] "
  indent n t =
    T.stripEnd $
      let spaces = T.replicate n " "
       in case T.lines t of
            (first : rest) ->
              T.unlines (first : map (\x -> spaces <> x) rest)
            _ -> t

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x : xs) = loop x xs
 where
  loop _ [] = True
  loop x (y : ys) = x == y && loop x ys

eitherFail :: MonadFail m => Either String a -> m a
eitherFail (Left err) = fail err
eitherFail (Right x) = pure x
