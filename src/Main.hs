{- This is the NetLion server. The server is
	responsible for mangaging the connections
	between clients as well as forwarding packets -}
module Main where
	import NetLion.Common
	import NetLion.Server
	import NetLion.Server.Actions
	import NetLion.Packets
	import NetLion.Packets.Serialize

	import Network
	import System.IO
	import Control.Concurrent

	import qualified Data.ByteString.Lazy as BSL
	import qualified Data.ByteString as BS
	import Data.Binary.Get
	import qualified Data.Serialize as S

	{- Returns an action to modify the server based
		on the packets recieved -}
	getPacketAction :: Handle -> Packet -> Server -> IO Server
	getPacketAction handle packet server =
		case packet of
			{- Reads a PingPacket -}
			PingPacket -> do
				-- signal to the terminal that it recieved a ping
				putStrLn "Pinged ..."
				return server

			{- Reads a PingAck, todo, add
				more implementation for this -}
			PingAck -> do
				putStrLn "PingAck ..."
				return server

			{- Handle a Connect Request from a Client -}
			ReqConnect clid -> do
				putStrLn $ "Adding client to server: " ++ clid
				serverAddClientIO (Client handle clid) server

			{- Handle a connect request ack -}
			ReqConnectAck int -> do
				putStrLn $ "Got req connect ack: " ++ (show int)
				return server

			{- Run a broadcast packet -}
			BroadcastPacket from to dat -> do
				putStrLn $ "Broadcasting datagram " ++ (show dat) ++ "" ++ " to " ++ (show to)
				serverBroadcastDatagram from dat to server

			BroadcastPacketAck -> do
				putStrLn $ "Broadcast acknowledged"
				return server

			Logout clid -> do
				putStrLn $ "Removing cleint " ++ clid
				serverRemoveClientIO clid server

			LogoutAck -> do
				putStrLn $ "Logout Acknowledged"
				return server

			_ -> do
				putStrLn $ "Not implemented"
				return server


			

	{- Reads the packets from a client.
		This function will monitor and wait for
		a reqConnect data packet to assign a client
		id to the connection -}
	readPackets :: Maybe ClientId -> Handle -> Server -> IO ()
	readPackets clid handle server@(Server _ m _) =
		let runPackets clid mvar handle = do
			putStrLn "Still running packets ..."
			{- If there's and EOF, we need to handle it by removing the
				client from the server and continuing -}
			eof <- hIsEOF handle
			if eof then do
				putStrLn "EOF Detected, attempting to logout user"
				case clid of
					{- If the client ID is specified, we need to
						log out the user -}
					Just c -> putMVar mvar (serverRemoveClientIO c)
					Nothing -> return ()
			else do
				{- Read a packet from the client -}
				packet <- readPacket handle
				case packet of
					(PacketResult _ (Success packet@(ReqConnect neclid))) -> do
						{- if the packet is a reqconnect packet, add it to the server -}
						putMVar mvar (getPacketAction handle packet)
						runPackets (Just neclid) mvar handle
					(PacketResult _ (Success packet)) -> do
						putMVar mvar (getPacketAction handle packet)
						runPackets clid mvar handle

		in runPackets clid m handle

	serverAccept :: Server -> IO ()
	serverAccept server@(Server _ _ (ServerConnectionData sock _)) =
		(accept sock) >>= (\(handle,_,_) -> do
				{- Accept a new handle to a client and fork a new
					thread that reads and sends packets -}
				putStrLn "Accepted connect, running . . ."
				_ <- forkIO $ readPackets Nothing handle server
				serverAccept server )
	
	main = do
		serverSock <- listenOn $ PortNumber 5434
		server <- initServer serverSock 5434

		putStrLn "Started server listening on port 5434"
		putStrLn "Forking runServer"

		threadId <- forkIO (runServer server >>= (\_ -> return ()))
		serverAccept server
		return ()

