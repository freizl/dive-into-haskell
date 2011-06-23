module Main where

{-- Try to find the file has max size under a dir recusively --}
{-- TODO
    1. error handlering
--}
    
import Control.Monad (forM, mapM)
import Control.Exception (handle, bracket)
import System.IO
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Environment (getArgs)

main = do 
    inps <- getArgs
    case inps of
      []   -> putStrLn "Input a Dirertory name"
      (xs) -> do
              files <- mapM getFilesInDir xs
              myFiles <- mapM getFileSize $ concat files
              print $ maximum myFiles

type FileSize = Integer
data MyFile   = MyFile {filePath :: FilePath, fileSize :: FileSize } deriving (Show)

instance Eq MyFile where
  MyFile x1 y1 == MyFile x2 y2 = x1 == x2 

instance Ord MyFile where
  MyFile x1 y1 `compare` MyFile x2 y2 = y1 `compare` y2
  
-- | get all files recursively under a dir.
-- Looks complex                   
-- NOTES: only one monad type during combination (>>=), therefore `return []` is type of `IO [FilePath]`
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir inp = do 
  isDir <- doesDirectoryExist inp
  files <- if isDir then
              (do
               names <- getDirectoryContents inp
               forM [ inp </> x | x <- names, isNotSpecialDir x ] getFilesInDir)
           else return [[inp]]                            -- why list of list?? the `then` condition return Monadic list of list
  return $ concat files

-- | get file size
getFileSize :: FilePath -> IO MyFile
getFileSize path = withFile path ReadMode
                   (\h -> do 
                          size <- hFileSize h
                          return $ MyFile path size)
             
-- | is not dir . or ..
isNotSpecialDir :: FilePath -> Bool
isNotSpecialDir x = x `notElem` [".", ".."]
