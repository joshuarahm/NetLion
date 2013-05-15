{- This executable is the daemon. Its job is to maintain a continuous
	connection to the server and sort through the i=nformation it
	recieves back.

	The daemon holds in its data structures a queue for each user
	that has tried to contact it. For example, it X sends a message m
	to Y, Y's daemon will store m in the queue mapped to X for later
	retrieval. -}
module Main where
	import Backlog
	import ChannelHandler

	import NetLion.Common
	import NetLion.Packets
	import NetLion.Packets.Serialize

	import Network

	import Control.Concurrent (forkIO)
	import Control.Monad

	import qualified Data.ByteString as BS
	import Data.Maybe

	import qualified Data.Map as Map
	import System.IO
	import Debug.Trace

	import System.Environment
	import Control.Concurrent.BoundedChan

	{- Some default values -}
	
	{- The path to the UNIX socket
		to listen on -}
	socketPath :: String
	socketPath = "/tmp/NetLion.sock"

	{- The block size to write to the server
		when the torrent of bit's comes streaming
		in -}
	blockSize :: (Integral a) => a
	blockSize = 1024

	{- This function will read a data packet from
		the server. And will append it to a backlog -}
	readFromServer :: Handle -> Backlog -> IO ()
	readFromServer handleToServer backlog = 
		trace "Reading data from server . . ." $ do
		{- Read a packet result from the server -}
		packetr <- readPacket handleToServer

		case packetr of
			{- If the packet result is a data packet -}
			(PacketResult _ (Success (DataPacket clid dat))) ->
				appendToBacklog clid dat backlog
			{- If the result is either a failure packet
				or not a data packet, fail -}
			(PacketResult _ (Fail s)) -> error s

			_ -> error "Incorrect paket type. Expected data packet"

	writeToServer :: Handle -> Backlog -> IO ()
	writeToServer handleToServer (Backlog _ buf _) =
		trace "Writing to server . . ." $ do
		packet <- readChan buf
		trace ("Forwarding packet " ++ (show packet)) $
			writePacket handleToServer packet


	handleReadReq :: String -> Handle -> Backlog -> Bool -> IO ()
	handleReadReq clid handle bl _ = do
		maychan <- backlogGetChan clid bl
		case maychan of
			-- dump a channel until the length of a bytestring is 0
			Just chan -> do
				flushChannel chan handle ()
			Nothing -> return ()

	handleWriteReq :: [String] -> Handle -> Backlog -> IO ()
	handleWriteReq clients handle bl@(Backlog _ buf name) = do
		-- read in a block of bytes. This should be one kilobyte
		bytes <- BS.hGetSome handle blockSize

		-- create a new broadcast packet
		let packet = (BroadcastPacket name clients bytes)
		
		-- write the packet to the server channel
		writeChan buf packet

		-- If the length is 0, then we're done here
		if BS.length bytes == 0 then return ()
		else handleWriteReq clients handle bl

	{- What this guy does is accept a connect, read the packet heading,
		if the client is reading, send it a torrent of data packets, if its
		writing, then block up the input into broadcast packets -}
	acceptConnect :: Socket -> Backlog -> IO ()
	acceptConnect sock bl =
		trace "accepted connection" $ do
		(clihandle,_,_) <- accept sock
		packet <- readPacket clihandle

		case packet of
			-- todo add support for global packet container
			(PacketResult _ (Success (ReadPacket maybecli follow ))) ->
				handleReadReq (fromMaybe "" maybecli) clihandle bl follow

			(PacketResult _ (Success (WritePacket clients))) ->
				handleWriteReq clients clihandle bl

			_ -> error "Invalid packet"
			
	useageString :: String
	useageString =
		"NetLion Client Daemon" ++
		"\n nlcd -user userid [-host hostid] [-port port] [-sock socket]" ++
		"\n\n -user userid" ++
		"\n  mandatory argument, speicfies what user to connect as" ++
		"\n -host hostid" ++
		"\n  Specifies the host to listent to; defaults to localhost" ++
		"\n -port port" ++
		"\n  Specifies the port to listen to; defaults to 5434" ++
		"\n -sock socket" ++
		"\n  socket to listen for clients on; defaults to " ++ socketPath

	main :: IO ()
	main = withSocketsDo $ do
		argmap <- getArgs >>= return . parseArgs

		case grabOneWithDefaults argmap
			[("user", Nothing),("host",Just "localhost"),
				("port",Just "5434"),("sock",Just socketPath)] of

			Success args -> do
				let user = args !! 0
				let host = args !! 1
				let port = (read $ args !! 2) :: Integer
				let sock = args !! 3

				serverSock <- listenOn $ UnixSocket sock
				handleToServer <- connectTo host (PortNumber $ fromIntegral port)
				backlog <- newBacklog user
		
				writePacket handleToServer (ReqConnect user)
		
				_ <- forkIO $ forever $ readFromServer handleToServer backlog
				_ <- forkIO $ forever $ writeToServer handleToServer backlog
		
				forever $ acceptConnect serverSock backlog

			Fail reason -> error $ reason ++ "\n" ++ useageString
