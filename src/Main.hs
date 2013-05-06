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

	getPacketAction :: Handle -> Packet -> Server -> IO Server
	getPacketAction handle packet server =
		case packet of
			PingPacket -> do
				putStrLn "Pinged ..."
				return server

			PingAck -> do
				putStrLn "PingAck ..."
				return server

			ReqConnect clid -> do
				putStrLn $ "Adding client to server: " ++ clid
				serverAddClientIO (Client handle clid) server

			ReqConnectAck int -> do
				putStrLn $ "Got req connect ack: " ++ (show int)
				return server

			BroadcastPacket from to dat -> do
				putStrLn $ "Broadcasting datagram " ++ (show dat) ++ "" ++ " to " ++ (show to)
				serverBroadcastDatagram dat to server

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


			

	readPackets :: Maybe ClientId -> Handle -> Server -> IO ()
	readPackets clid handle server@(Server _ m _) =

		let readPacketChunk :: Get BS.ByteString ; readPacketChunk =
			getWord32be >>= getByteString . fromIntegral
		in

		let runPackets clid mvar handle = do
			eof <- hIsEOF handle
			if eof then do
				putStrLn "EOF Detected, attempting to logout user"
				case clid of
					Just c -> putMVar mvar (serverRemoveClientIO c)
					Nothing -> return ()
			else do
				chunk <- (BSL.hGetContents handle) >>= return . (runGet readPacketChunk)
	
				case (S.decode chunk) of
					Right (PacketResult _ (Success packet@(ReqConnect neclid))) -> do
						putMVar mvar (getPacketAction handle packet)
						runPackets (Just neclid) mvar handle
					Right (PacketResult _ (Success packet)) -> do
						putMVar mvar (getPacketAction handle packet)
						runPackets clid mvar handle
					Left _ ->
						runPackets clid mvar handle

		in runPackets clid m handle

	serverAccept :: Server -> IO ()
	serverAccept server@(Server _ _ (ServerConnectionData sock _)) =
		(accept sock) >>= (\(handle,_,_) -> do
				putStrLn "Accepted connect, running . . ."
				forkIO $ readPackets Nothing handle server
				serverAccept server )
	
	main = do
		serverSock <- listenOn $ PortNumber 5434
		server <- initServer serverSock 5434

		putStrLn "Started server listening on port 5434"
		putStrLn "Forking runServer"

		threadId <- forkIO (runServer server >>= (\_ -> return ()))
		serverAccept server
		return ()

