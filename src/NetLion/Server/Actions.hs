module NetLion.Server.Actions where
	import NetLion.Common
	import NetLion.Server

	import NetLion.Packets.Serialize
	import NetLion.Packets
	import Data.Serialize as S

	import qualified Data.ByteString as BS
	import qualified System.IO as IO

	{- Add a client to the server -} 
	serverAddClientIO :: Client -> Server -> IO Server
	serverAddClientIO client server =
		return $ serverAddClient client server
	
	serverRemoveClientIO :: ClientId -> Server -> IO Server
	serverRemoveClientIO client =
		return . (serverRemoveClient client)

	serverBroadcastDatagram :: ClientId -> Datagram -> [ClientId] -> Server -> IO Server
	serverBroadcastDatagram from datagram clients server =
		forAll clients server (\ id ->
			case (serverGetClientById id server) of
				Just (Client handle id) ->
					writePacket handle (DataPacket from datagram)
				_ -> putStrLn $ "Can't forward to " ++ id ++ ", no such client"
		)
