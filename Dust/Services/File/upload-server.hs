import System.IO.Error
import Network.Socket (Socket)
import qualified Data.ByteString as B
import Data.Serialize

import Dust.Network.Util
import Dust.Model.TrafficModel
import Dust.Model.Observations
import Dust.Services.Shaper.Shaper
import Dust.Crypto.PRNG
import Dust.Crypto.Keys
import Dust.Crypto.ECDSA
import Dust.Crypto.ECDH
import Dust.Core.CryptoProtocol (Session(..), makeSession)
import Dust.Crypto.DustCipher
import Dust.Core.DustPacket
import Dust.Core.WireProtocolHandler
import Dust.Core.CryptoProtocol
import Dust.Network.ProtocolSocket
import Dust.Network.TcpServer
import Dust.Services.File.File
import Dust.Core.SignedDustPacket

main :: IO()
main = do
    rand <- newPRNG

    putStrLn "Loading keys..."
    (keypair, newKeys, rand') <- ensureKeys rand

    if newKeys
        then putStrLn "Generating new keys..."
        else putStrLn "Loaded keys."

    let host = "0.0.0.0"
    let port = 9521

    server host port (handleRequest keypair)

ensureKeys :: DustPRNG -> IO (Keypair, Bool, DustPRNG)
ensureKeys rand = do
    result <- tryIOError loadKeypair
    case result of
        Left e -> do
            let (bytes, rand') = randomBytes 32 rand
            keys <- createKeypair
            saveKeypair keys
            return (keys, True, rand')
        Right keypair -> return (keypair, False, rand)

handleRequest :: Keypair -> Socket -> IO()
handleRequest keypair sock = do
    putStrLn "Handling connection..."
    (session, buffer) <- getSession B.empty keypair sock
    if confirmSession session
        then do
            maybeHeader <- getHeader session sock
            case maybeHeader of
                Just header -> writeChunks session sock header
        else putStrLn $ "Bad session " ++ show session

getHeader :: Session -> Socket -> IO (Maybe FileHeader)
getHeader session sock = do
    (plaintext@(Plaintext payload), buffer') <- getPacket B.empty session sock
    let maybeVerified = verifyEncodedMessage payload
    case maybeVerified of
        Nothing             -> do
            putStrLn "Verification failed"
            return Nothing
        Just (public, (Plaintext file)) -> do
            putStrLn $ "Upload verified"
            let eitherHeader = (decode file) :: (Either String FileHeader)
            case eitherHeader of
                Left err -> do
                    putStrLn $ "Error decoding file " ++ err
                    return Nothing
                Right header@(FileHeader name size) -> do
                    putStrLn $ "Writing file " ++ name ++ show size
                    openFile header
                    return $ Just header

writeChunks :: Session -> Socket -> FileHeader -> IO()
writeChunks session sock (FileHeader _ 0) = return ()
writeChunks session sock header@(FileHeader name size) = do
    putStrLn $ "Writing chunk " ++ name ++ show size
    (plaintext@(Plaintext payload), buffer') <- getPacket B.empty session sock
    let maybeVerified = verifyEncodedMessage payload
    case maybeVerified of
        Nothing             -> do
            putStrLn "Verification failed"
            return ()
        Just (public, (Plaintext file)) -> do
            putStrLn $ "Upload verified"
            let eitherFiledata = (decode file) :: (Either String FileData)
            case eitherFiledata of
                Left err -> putStrLn $ "Error decoding file " ++ err
                Right filedata -> do
                    writeFileData header filedata
                    writeChunks session sock $ FileHeader name (size-1)
