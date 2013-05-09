{- This program is the NetLion client utility. While any program can use the
	protocol to query information from NetLion, this program makes it the easiest
	by simply allowing the user to pipe to and from it easily -}
module Main where
	import Network
	import System.IO 

	import System.Environment
	import qualified Data.ByteString as BS

	import NetLion.Common
	import NetLion.Packets
	import NetLion.Packets.Serialize

	{- Default path of the daemon -}
	socketPath :: String
	socketPath = "/tmp/NetLion.sock"

	{- Redirects all information read from the first handle
		and redirects it to the second handle -}
	writeAll h1 h2 = do
		
		{- Check to see if the handle is
			a end of file -}
		iseof <- hIsEOF h1
		if iseof then return () else do
			
			{- Read some bytes from h1 and write those bytes
				to h2 -}
			(BS.hGetSome h1 1024) >>= (BS.hPutStr h2)

			{- Recusively call write all again -}
			writeAll h1 h2

	{- Handle the case where the user
		wants to write infomation from
		stdin this takes two arguments:

		A path to the socket and a list
		of who to broadcast to -}
	handleWrite :: String -> [String] -> IO ()
	handleWrite spath tos = do
		{- Connect to the daemon through its socket -}
		handle <- connectTo "localhost" (UnixSocket spath)

		{- Tell the daemon that this client would like
			to write to the server -}
		writePacket handle (WritePacket tos)

		{- Write all the information from stdin
			to the open socket -}
		writeAll stdin handle

		{- Flush and close the handle -}
		hFlush handle
		hClose handle

	{- Handles the case where the user wants to read infoamtion
		from the daemon and write the output to
		stdout 
		
		This function takes 3 arguments:
		A path to the socket, a client name to
		pull information from and a boolean that tells
		wheather to follow or close after read -}
	handleRead :: String -> String -> Bool -> IO ()
	handleRead spath from follow = do
		
		{- connect to the socket to the daemon -}
		handle <- connectTo "localhost" (UnixSocket spath)

		{- Tell the daemon that we would like to read -}
		writePacket handle (ReadPacket (Just from) follow)

		{- Writes all the information from the daemon to
			stdout -}
		writeAll handle stdout
		hFlush stdout
		
	
	main = withSocketsDo $ do
		{- parse the argument map -}
		argmap <- getArgs >>= return . parseArgs

		let maybeRun = do

			{- Pull to socket location from the arguments -}
			(spath:_) <- grabOneWithDefaults argmap [("sockfile",Just socketPath)]

			{- Get whether or not the follow switch exists -}
			follow <- grabExists "follow" argmap

			{- Try to grab to or from arguments from lsit -}
			case (grab "to" argmap, grabOne "from" argmap) of
				{- There must be one or the other -}
				(Fail _, Fail _) -> Fail "Missing either to or from arguments!"

				{- Handle the case for one existing -}
				(Success s, Fail _) -> Success (spath, Just s, Nothing, follow)
				(Fail _, Success s) -> Success (spath, Nothing, Just s, follow)

				{- May not have both swiches; this is a read xor write
					program -}
				_ -> Fail "May not have both -to and -from"
			
		{- Match to see if the arguments are
			all there -}
		case maybeRun of
			{- Handle read and writes -}
			Success (spath, Just tos, _, _) -> handleWrite spath tos
			Success (spath, _, Just from, follow) -> handleRead spath from follow

			{- If the arguments didn't parse, fail and tell why -}
			Fail reason -> error reason
