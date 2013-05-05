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


			

	readPackets :: Handle -> Server -> IO ()
	readPackets handle server@(Server _ m _) =

		let readPacketChunk :: Get BS.ByteString ; readPacketChunk =
			getWord32be >>= getByteString . fromIntegral
		in

		let runPackets mvar handle = do
			chunk <- (BSL.hGetContents handle) >>= return . (runGet readPacketChunk)

			case (S.decode chunk) of
				Right (PacketResult _ (Success packet)) -> do
					putMVar mvar (getPacketAction handle packet)
					runPackets mvar handle
				Left _ ->
					runPackets mvar handle

		in runPackets m handle

	serverAccept :: Server -> IO ThreadId
	serverAccept server@(Server _ _ (ServerConnectionData sock _)) =
		(accept sock) >>= (\(handle,_,_) ->
				forkIO $ readPackets handle server )
	
	main = do
		serverSock <- listenOn $ PortNumber 5434
		server <- initServer serverSock 5434

		threadId <- forkIO (runServer server >>= (\_ -> return ()))
		return ()

