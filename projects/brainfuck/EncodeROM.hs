module Main where

import Clash.Prelude
import qualified Data.ByteString as BS
import System.Environment
import qualified Data.List as L
import Data.Word

createRomFile :: FilePath -> FilePath -> IO ()
createRomFile fileR fileW = do
    bs <- BS.unpack <$> BS.readFile fileR
    bs <- return $ L.take 1024 $ bs <> L.repeat 0
    let bvs = L.map (L.filter (/= '_') . show . pack) bs
    writeFile fileW (unlines bvs)

main :: IO ()
main = do
  [fileR,fileW] <- getArgs
  createRomFile fileR fileW
