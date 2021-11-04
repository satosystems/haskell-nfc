module Lib
  ( main'
  ) where

import           Control.Monad                  ( when )
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import qualified Data.Text                     as T
import           Data.Text.Normalize            ( NormalizationMode(NFC)
                                                , normalize
                                                )
import           System.Directory               ( listDirectory
                                                , renameFile
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )
import           System.FilePath.Posix          ( pathSeparator )

toNFC :: FilePath -> Maybe (FilePath, FilePath)
toNFC fileName = if original == normalized
  then Nothing
  else Just (T.unpack original, T.unpack normalized)
 where
  original   = T.pack fileName
  normalized = normalize NFC original

main' :: IO ()
main' = do
  args <- getArgs
  null args `when` exitFailure
  let dir  = head args
  let dir' = if last dir == pathSeparator then dir else dir ++ [pathSeparator]
  files <- listDirectory dir'
  let addPath mp = let p = fromJust mp in bimap (dir' ++) (dir' ++) p
  let targets = map addPath . filter isJust $ map toNFC files
  mapM_ (uncurry renameFile) targets
