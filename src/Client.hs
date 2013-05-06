module Main where
	import NetLion.Common
	import NetLion.Packets
	import NetLion.Packets.Serialize

	import Network

	import Control.Concurrent (forkIO)
	import Control.Concurrent.BoundedChan
	import Control.Concurrent.MVar
	import Control.Monad

	import qualified Data.ByteString.Lazy as BSL
	import qualified Data.ByteString as BS
	import qualified Data.Serialize as S
	import Data.Maybe
	import Data.Binary.Get

	import qualified Data.Map as Map
	import System.IO
	import Debug.Trace

	{- todo un-hardcode this -}
	socketPath :: String
	socketPath = "/tmp/NetLion.sock"

	blockSize :: (Integral a) => a
	blockSize = 1024

	maxBacklog :: (Integral a) => a
	maxBacklog = 1024

	readPacketFromHandle :: Handle -> IO PacketResult
	readPacketFromHandle handle =
		let readPacketChunk :: Get BS.ByteString ; readPacketChunk =
			getWord32be >>= getByteString . fromIntegral
		in do
		chunk <- (BSL.hGetContents handle) >>= return . (runGet readPacketChunk)
		case (S.decode chunk) of
			Right pr -> return pr
			Left err -> error err

	data Backlog = Backlog (MVar ( Map.Map String (BoundedChan BS.ByteString ))) (BoundedChan Packet) String

	newBacklog :: String -> IO Backlog
	newBacklog name = do
		mvar <- newMVar Map.empty
		-- this is for the server writes
		bc <- newBoundedChan maxBacklog
		return (Backlog mvar bc name)

	appendToBacklog :: String -> BS.ByteString -> Backlog -> IO ()
	appendToBacklog clid dat (Backlog mvar _ _) =
		trace ("Appending data to backlog") $

		{- To append to the backlog, we need
			to modify the mutable variable for the Map -}
		modifyMVar_ mvar (\m ->
			{- Check to see if there is already
				a channel for the client id -}
			case Map.lookup clid m of
				Just chan -> do
					{- if there is, then we can
						just write to that channel -}
					writeChan chan dat
					return m
				Nothing -> do
					{- if not, then we have to explicitly
						go through and create a new channel
						and map it to that client id -}
					chan <- newBoundedChan maxBacklog
					writeChan chan dat
					return $ Map.insert clid chan m)



	{- This function will read a data packet from
		the server. And will append it to a backlog -}
	readFromServer :: Handle -> Backlog -> IO ()
	readFromServer handleToServer backlog = 
		trace "Reading data from server . . ." $ do
		{- Read a packet result from the server -}
		packetr <- readPacketFromHandle handleToServer

		case packetr of
			{- If the packet result is a data packet -}
			(PacketResult _ (Success (DataPacket clid dat))) ->
				appendToBacklog clid dat backlog
			{- If the result is either a failure packet
				or not a data packet, fail -}
			(PacketResult _ (Fail s)) -> error s

			_ -> error "Incorrect paket type. Expected data packet"

	writeToServer handleToServer (Backlog _ buf _) =
		trace "Writing to server . . ." $ do
		packet <- readChan buf
		BS.hPutStr handleToServer (S.encode (packetToPacketResult packet))

	backlogGetChan :: String -> Backlog -> IO (Maybe (BoundedChan BS.ByteString))
	backlogGetChan clid (Backlog mvar _ _ ) =
		withMVar mvar (\clmap -> return (Map.lookup clid clmap) )

	dumpChannelUntil :: (BS.ByteString -> Bool) -> BoundedChan BS.ByteString-> Handle -> IO ()
	dumpChannelUntil cond chan handle = do
		bs <- readChan chan
		iseof <- hIsEOF handle
		if (cond bs) || iseof then return ()
		else BS.hPutStr handle bs

	handleReadReq :: String -> Handle -> Backlog -> IO ()
	handleReadReq clid handle bl = do
		maychan <- backlogGetChan clid bl
		case maychan of
			-- dump a channel until the length of a bytestring is 0
			Just chan -> dumpChannelUntil (((==) 0) . BS.length) chan handle
			Nothing -> return ()

	handleWriteReq :: [String] -> Handle -> Backlog -> IO ()
	handleWriteReq clients handle bl@(Backlog _ buf name) = do
		bytes <- BS.hGetSome handle blockSize
		writeChan buf (BroadcastPacket name clients bytes)
		if BS.length bytes == 0 then return ()
		else handleWriteReq clients handle bl

	{- What this guy does is accept a connect, read the packet heading,
		if the client is reading, send it a torrent of data packets, if its
		writing, then block up the input into broadcast packets -}
	acceptConnect :: Socket -> Backlog -> IO ()
	acceptConnect sock bl = do
		(clihandle,hostname,portnum) <- accept sock
		packet <- readPacketFromHandle clihandle

		case packet of
			-- todo add support for global packet container
			(PacketResult _ (Success (ReadPacket maybecli))) ->
				handleReadReq (fromMaybe "" maybecli) clihandle bl

			(PacketResult _ (Success (WritePacket clients))) ->
				handleWriteReq clients clihandle bl

			_ -> error "Invalid packet"
			
	main = withSocketsDo $ do
		{- todo unhardcode these things -}
		serverSock <- listenOn $ UnixSocket socketPath
		handleToServer <- connectTo "localhost" (PortNumber 5434)
		backlog <- newBacklog "jrahm"

		forkIO $ forever $ readFromServer handleToServer backlog
		forkIO $ forever $ writeToServer handleToServer backlog

		forever $ acceptConnect serverSock backlog
