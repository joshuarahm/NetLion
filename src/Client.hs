module Main where
	import Network
	import qualified Data.ByteString as BS
	import System.IO 

	import NetLion.Packets
	import NetLion.Packets.Serialize

	import NetLion.Common
	import System.Environment

	socketPath :: String
	socketPath = "/tmp/NetLion.sock"

	writeAll h1 h2 = do
		iseof <- hIsEOF h1
		if iseof then return () else do
			(BS.hGetSome h1 1024) >>= (BS.hPutStr h2)
			writeAll h1 h2

	handleWrite :: String -> [String] -> IO ()
	handleWrite spath tos = do
		handle <- connectTo "localhost" (UnixSocket spath)
		writePacket handle (WritePacket tos)
		writeAll stdin handle
		hFlush handle
		hClose handle

	handleRead :: String -> String -> Bool -> IO ()
	handleRead spath from follow = do
		handle <- connectTo "localhost" (UnixSocket spath)
		writePacket handle (ReadPacket (Just from) follow)
		writeAll handle stdout
		hFlush stdout
		
	
	main = withSocketsDo $ do
		argmap <- getArgs >>= return . parseArgs

		let maybeRun = do
			(spath:_) <- grabOneWithDefaults argmap [("sockfile",Just socketPath)]
			follow <- grabExists "follow" argmap
			case (grab "to" argmap, grabOne "from" argmap) of
				(Fail _, Fail _) -> Fail "Missing either to or from arguments!"
				(Success s, Fail _) -> Success (spath, Just s, Nothing, follow)
				(Fail _, Success s) -> Success (spath, Nothing, Just s, follow)
				_ -> Fail "May not have both -to and -from"
			
		case maybeRun of
			Success (spath, Just tos, _, _) -> handleWrite spath tos
			Success (spath, _, Just from, follow) ->
				handleRead spath from follow
			Fail reason -> error reason
