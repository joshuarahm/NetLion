module NetLion.Server.Actions where
	import NetLion.Common
	import NetLion.Server

	import NetLion.Packets.Serialize
	import NetLion.Packets


	{- Add a client to the server -} 
	serverAddClientIO :: Client -> Server -> IO Server
	serverAddClientIO client server =
		return $ serverAddClient client server
	
	serverRemoveClientIO :: ClientId -> Server -> IO Server
	serverRemoveClientIO client =
		return . (serverRemoveClient client)

	serverBroadcastDatagram :: ClientId -> Datagram -> [ClientId] -> Server -> IO Server
	serverBroadcastDatagram from datagram clients server =
		forAll clients server (\ identity ->
			case (serverGetClientById identity server) of
				Just (Client handle _) ->
					writePacket handle (DataPacket from datagram)
				_ -> putStrLn $ "Can't forward to " ++ identity ++ ", no such client"
		)
