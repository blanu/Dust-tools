{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Services.File.File
(
    File(..),
    FileHeader(..),
    FileData(..),
    readEncodedFile,
    openFile,
    writeFileData
)
where

import GHC.Generics
import Data.Serialize
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List.Split
import System.Directory

data File = File FileHeader [FileData] deriving (Generic, Show, Eq) -- name contents
instance Serialize File

data FileHeader = FileHeader String Int deriving (Generic, Show, Eq) -- filename count
instance Serialize FileHeader

data FileData = FileData ByteString deriving (Generic, Show, Eq)
instance Serialize FileData

readEncodedFile :: String -> IO File
readEncodedFile path = do
    let name = sanitize path
    contents <- B.readFile path
    let chunks = splitData 1000 contents
    let filedata = map FileData chunks
    let header = FileHeader name $ length chunks
    return $ File header filedata

splitData :: Int -> ByteString -> [ByteString]
splitData size bs =
    if B.length bs < size
        then [bs]
        else (B.take size bs) : splitData size (B.drop size bs)

openFile :: FileHeader -> IO ()
openFile (FileHeader path _) = do
    let name = sanitize path
    removeFile $ "incoming/"++name

writeFileData :: FileHeader -> FileData -> IO()
writeFileData (FileHeader path _) (FileData contents) = do
    let name = sanitize path
    B.appendFile ("incoming/"++name) contents

sanitize :: String -> String
sanitize path = do
    if elem '/' path
        then last $ splitOn "/" path
        else path
