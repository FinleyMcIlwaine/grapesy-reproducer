{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

module Client where

import Network.GRPC.Spec
import Network.GRPC.Client
import Network.GRPC.Common

import Control.Exception
import Data.ByteString.Lazy.Char8 qualified as BS.Char8
import Data.IORef
import System.Environment
import System.IO.Unsafe

type ReproRpc = RawRpc "repro" "repro"
type instance RequestMetadata          ReproRpc = NoMetadata
type instance ResponseInitialMetadata  ReproRpc = NoMetadata
type instance ResponseTrailingMetadata ReproRpc = NoMetadata

reproClient :: IO ()
reproClient = do
    numMessages <- read @Word . head <$> getArgs
    let
      streamFromServer =
        withConnection def server $ \conn -> do
          withRPC conn def (Proxy @ReproRpc) $ \call -> do
            sendFinalInput call (BS.Char8.pack $ show numMessages)
            recvAll call
    streamFromServer `finally` do
      numReceived <- readIORef numReceivedRef
      putStrLn $ "Received " ++ show numReceived ++ " stream elements"
  where
    server :: Server
    server =
        ServerInsecure $ Address
          { addressHost      = "localhost"
          , addressPort      = defaultInsecurePort
          , addressAuthority = Nothing
          }

    recvAll :: Network.GRPC.Client.Call ReproRpc -> IO ()
    recvAll call = do
        out <- recvOutput call
        case out of
          StreamElem  _   -> inc >> recvAll call
          FinalElem   _ _ -> inc
          NoMoreElems _   -> return ()
      where
        inc = modifyIORef' numReceivedRef (+1)

{-# NOINLINE numReceivedRef #-}
numReceivedRef :: IORef Word
numReceivedRef = unsafePerformIO $ newIORef 0
