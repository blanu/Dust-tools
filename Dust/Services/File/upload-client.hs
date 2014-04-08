import System.IO
import System.IO.Error
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Text.Printf (printf)
import Data.List as L
import Control.Concurrent
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.Entropy
import Control.Exception
import Data.Word (Word16)
import System.Environment (getArgs)
import Dust.Services.File.Progress
import System.IO

import Dust.Network.Util
import Dust.Model.TrafficModel
import Dust.Model.Observations
import Dust.Services.Shaper.Shaper
import Dust.Crypto.PRNG
import Dust.Crypto.Keys
import Dust.Crypto.ECDSA
import Dust.Crypto.ECDH
import Dust.Core.CryptoProtocol (Session(..), makeSession)
import Dust.Core.WireProtocol
import Dust.Crypto.DustCipher
import Dust.Core.DustPacket
import Dust.Core.WireProtocolHandler
import Dust.Network.ProtocolSocket
import Dust.Services.File.File
import Dust.Core.SignedDustPacket

main :: IO()
main = do
    args <- getArgs

    case args of
        (host:filepath:_) -> upload host filepath
        otherwise    -> do
            putStrLn "Usage:   upload <host> <filepath>"
            putStrLn "Example: upload 74.125.227.196 report.txt"

upload :: String -> String -> IO()
upload host path = do
    rand <- newPRNG

    putStrLn "Loading signing keys..."
    (signingKeypair, newKeys, rand') <- ensureKeys rand

    if newKeys
        then putStrLn "Generating new signing keys..."
        else putStrLn "Loaded signing keys."

    keypair <- createKeypair
    public <- loadPublic "id.pub"
    let (iv, rand'') = createIV rand'
    let session = makeSession keypair public iv

    let port = 9521

    file <- readEncodedFile path
    let payload = makeEncodedMessage False (Plaintext file) (Just signingKeypair)

    client host port (handleRequest session (Plaintext payload))

ensureKeys :: DustPRNG -> IO (Keypair, Bool, DustPRNG)
ensureKeys rand = do
    result <- tryIOError loadSigningKeypair
    case result of
        Left e -> do
            let (bytes, rand') = randomBytes 32 rand
            keys <- createSigningKeypair
            saveSigningKeypair keys
            return (keys, True, rand')
        Right keypair -> return (keypair, False, rand)

client :: String -> PortNumber -> (Socket -> IO()) -> IO()
client host port handleRequest = withSocketsDo $ do
        sock <- socket AF_INET Stream defaultProtocol
        addr <- inet_addr host
        connect sock (SockAddrInet port addr)
        setSocketOption sock NoDelay 1

        handleRequest sock

handleRequest :: Session -> Plaintext -> Socket -> IO()
handleRequest session payload sock = do
    encodeWithProgress session payload sock progressBar
    hPutChar stderr '\n'
