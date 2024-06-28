{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Network.GRPC.Spec
import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS.Char8

type ReproRpc = RawRpc "repro" "repro"
type instance RequestMetadata          ReproRpc = NoMetadata
type instance ResponseInitialMetadata  ReproRpc = NoMetadata
type instance ResponseTrailingMetadata ReproRpc = NoMetadata

reproServer :: IO ()
reproServer = do
    runServerWithHandlers serverConfig serverParams
      [ SomeRpcHandler (Proxy @ReproRpc) $
          streamingRpcHandler $
            mkServerStreaming $ \inp send -> do
              let n = read $ BS.Char8.unpack inp
              replicateM_ n $ send "0000000000000000000000000000000000000000000000000"
      ]
  where
    serverConfig :: ServerConfig
    serverConfig =
        ServerConfig
          { serverInsecure = Just $ InsecureConfig Nothing defaultInsecurePort
          , serverSecure   = Nothing
          }

    serverParams :: ServerParams
    serverParams = def { serverCompression = Compr.none }
