import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import Data.Aeson

import Dust.Model.TrafficModel

main = do
  args <- getArgs

  case args of
    (jsonpath:modelpath:_) -> importJson jsonpath modelpath
    otherwise      -> putStrLn "Usage: import [file.json] [file.model]"

instance FromJSON ProtocolModel
instance FromJSON TrafficModel

importJson :: FilePath -> FilePath -> IO ()
importJson jsonpath modelpath = do
  putStrLn $ "Loading " ++ jsonpath
  s <- BL.readFile jsonpath
  let maybeModel = (decode s) :: Maybe ProtocolModel
  case maybeModel of
    Just model -> do
      putStrLn $ "Writing " ++ modelpath
      saveModel model modelpath
      putStrLn "Done"
    Nothing -> do
      putStrLn "Failure to parse"
      return ()
