{-# LANGUAGE DeriveGeneric, DefaultSignatures #-} -- For automatic generation of cereal put and get

module Dust.Services.File.File
(
    File(..),
    readEncodedFile,
    writeEncodedFile
)
where

import GHC.Generics
import Data.Serialize
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List.Split

data File = File String ByteString deriving (Generic, Show, Eq) -- name contents
instance Serialize File

readEncodedFile :: String -> IO ByteString
readEncodedFile path = do
    contents <- B.readFile path
    let name = sanitize path
    let file = File name contents
    return $ encode file

writeEncodedFile :: ByteString -> IO ()
writeEncodedFile bs = do
    let eitherFile = (decode bs) :: (Either String File)
    case eitherFile of
        Left error                 -> putStrLn $ "Error parsing File: " ++ error
        Right (File path contents) -> do
            let name = sanitize path
            B.writeFile ("incoming/"++name) contents

sanitize :: String -> String
sanitize path = do
    if elem '/' path
        then last $ splitOn "/" path
        else path
