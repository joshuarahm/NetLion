{- This executable is the daemon. Its job is to maintain a continuous
	connection to the server and sort through the i=nformation it
	recieves back.

	The daemon holds in its data structures a queue for each user
	that has tried to contact it. For example, it X sends a message m
	to Y, Y's daemon will store m in the queue mapped to X for later
	retrieval. -}
module Main where
	import NetLion.Common
	import NetLion.Packets
	import NetLion.Packets.Serialize

	import Network

	import Control.Concurrent (forkIO)
	import Control.Concurrent.BoundedChan
	import Control.Concurrent.MVar
	import Control.Monad

	import qualified Data.ByteString as BS
	import Data.Maybe

	import qualified Data.Map as Map
	import System.IO
	import Debug.Trace

	import System.Environment

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

	{- The maximum number of data packets to
		retain in a user's queue without
		blocking. This is set to a megabyte per
		user -}
	maxBacklog :: (Integral a) => a
	maxBacklog = 1024


	{- A mutable data structure that holds the queue's for
		each contact as well as a queue to post information
		to the server on and a user name to register to the
		server with -}
	data Backlog = Backlog
		(MVar ( Map.Map String (BoundedChan BS.ByteString )))
		(BoundedChan Packet) String

	{- Creates a new Backlog with a username -}
	newBacklog :: String -> IO Backlog
	newBacklog name = do
		-- create new mutable variables
		mvar <- newMVar Map.empty
		-- this is for the server writes
		bc <- newBoundedChan maxBacklog
		return (Backlog mvar bc name)

	appendToBacklog :: String -> BS.ByteString -> Backlog -> IO ()
	appendToBacklog clid dat (Backlog mvar _ _) =
		trace ("Appending data to backlog: clid=" ++ clid ++ " dat=" ++ (show dat)) $

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

	backlogGetChan :: String -> Backlog -> IO (Maybe (BoundedChan BS.ByteString))
	backlogGetChan clid (Backlog mvar _ _ ) =
		withMVar mvar (\clmap -> return (Map.lookup clid clmap) )

	dumpChannelUntil :: (BS.ByteString -> Bool) -> BoundedChan BS.ByteString-> Handle -> IO ()
	dumpChannelUntil cond chan handle = do
		putStrLn $ "Trying to figure out if handle is closed"
		iseof <- hIsClosed handle
		putStrLn $ "Handle closed? " ++ (show iseof)
		isempty <- isEmptyChan chan
		if iseof || isempty then trace "Handle is EOF!" $ return ()
		else do
			putStrLn $ "Reading channel"
			bs <- readChan chan
			putStrLn $ "Read ByteString " ++ (show bs) ++ " from channel"
			if (cond bs) then return ()
			else do
				BS.hPutStr handle bs
				dumpChannelUntil cond chan handle

	handleReadReq :: String -> Handle -> Backlog -> Bool -> IO ()
	handleReadReq clid handle bl _ = do
		maychan <- backlogGetChan clid bl
		case maychan of
			-- dump a channel until the length of a bytestring is 0
			Just chan -> do
				putStrLn "Dumping channel to handle"
				dumpChannelUntil (\_ -> False) chan handle
				putStrLn "Condition met, finishing"
			Nothing -> return ()

	handleWriteReq :: [String] -> Handle -> Backlog -> IO ()
	handleWriteReq clients handle bl@(Backlog _ buf name) = 
		trace "Handling write request" $ do
		bytes <- BS.hGetSome handle blockSize
		let packet = (BroadcastPacket name clients bytes)
		trace ("writing packet " ++ (show packet)) $
			writeChan buf packet
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
