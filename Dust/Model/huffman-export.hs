import System.Environment (getArgs)
import System.IO.Error
import Data.List as L hiding (map)
import Data.Word
import Data.Map as M hiding (map)

import Dust.Model.Huffman

main = do
    args <- getArgs

    case args of
        (modelpath:exportpath:_) -> huffman modelpath exportpath
        otherwise      -> putStrLn "Usage: huffman [modelpath] [exportpath]"

huffman :: FilePath -> FilePath -> IO()
huffman modelpath exportpath = do
    freqs <- loadFrequencies modelpath
    let huffman = countsToTree freqs
    let text = export huffman
    writeFile exportpath text

loadFrequencies :: String -> IO [(Word8,Int)]
loadFrequencies modelpath = do
  content <- readFile modelpath
  let l = lines content
  return $ parseLines 0 l

parseLines :: Word8 -> [String] -> [(Word8,Int)]
parseLines _ [] = []
parseLines w (is:lines) =
  let i = (read is) :: Int
  in (w, i) : parseLines (w+1) lines

export :: HuffmanTree Word8 -> String
export tree =
  let codeMap = codes tree
      keys = [0..255] :: [Word8]
      vals = map (stringify codeMap) keys
  in unlines vals

stringify :: M.Map Word8 [Bool] -> Word8 -> String
stringify map key =
  let maybeVal = M.lookup key map
  in case maybeVal of
    Nothing -> ""
    Just val -> toBitstring val

toBitstring :: [Bool] -> String
toBitstring [] = ""
toBitstring (bit:bits) =
  case bit of
    False -> "0" ++ toBitstring bits
    True -> "1" ++ toBitstring bits
