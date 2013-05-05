module NetLion.Server.Actions where
	import NetLion.Common
	import NetLion.Server

	import qualified Data.ByteString as BS
	import qualified System.IO as IO

	serverAddClientIO :: Client -> Server -> IO Server
	serverAddClientIO client server =
		return $ serverAddClient client server
	
	serverRemoveClientIO :: ClientId -> Server -> IO Server
	serverRemoveClientIO client =
		return . (serverRemoveClient client)

	serverBroadcastDatagram :: Datagram -> [ClientId] -> Server -> IO Server
	serverBroadcastDatagram datagram clients server =
		forAll clients server (\ id ->
			case (serverGetClientById id server) of
				Just (Client handle id) -> BS.hPutStr handle datagram
				_ -> putStrLn $ "Can't forward to " ++ id ++ ", no such client"
		)
