module Main where
	import NetLion.Packets
	import NetLion.Packets.Serialize
	import NetLion.Common 

	import qualified Data.Serialize as S
	import Data.String
	import qualified Data.Map as Map
	import Data.List

	import System.Environment
	import System.IO
	import Debug.Trace
	import Data.Serialize.Put
	import qualified Data.ByteString as BS

	packetTypeMap :: Map.Map String PacketType
	packetTypeMap =
		Map.fromList [
			("ping",PingPacketType),
			("pingack",PingAckType),
			("reqconnect",ReqConnectType),
			("reqconnectack",ReqConnectAckType),
			("bcast",BroadcastPacketType),
			("bcastack",BroadcastPacketAckType),
			("logout",LogoutType),
			("logoutack",LogoutAckType),
			("data",DataPacketType) ];
	-- -t bcast --from josh --to nic zach --data "Hello There People"
	-- -t connect --from josh
	splitList :: (a -> Bool) -> [a] -> [[a]]
	splitList func list =
		case break func list of
			(la,[]) -> [la]
			(la,(a:as)) -> case (splitList func as) of
				(b:bs) -> la : (a:b) : bs
				l -> (la ++ [a]) : l

	parseArgs :: [String] -> Map.Map String [String]
	parseArgs args =
		let spltlist = filter (not . null) $ splitList (isPrefixOf "-") args in
		trace ("spltlist=" ++ (show spltlist)) $
		foldl (\acc e ->
			case e of
				(a:as) -> Map.insert (tail a) as acc
				_ -> acc ) Map.empty spltlist
	
	packetTypeFromString :: String -> Result PacketType
	packetTypeFromString str =
		case Map.lookup str packetTypeMap of
			Just a -> Success a
			Nothing -> Fail $ "Invalid packet type " ++ str

	packetTypeFromMap :: Map.Map String [String] -> Result PacketType
	packetTypeFromMap args =
		case Map.lookup "type" args of
			Just x ->
				case x of
					(a:[]) -> packetTypeFromString a
					_ -> Fail $ "Only one packet type allowed! " ++ (show x)
			Nothing -> Fail $ "No packet type specified"
			
	grab :: String -> Map.Map String [String] -> Result [String] ;
	grab str m = case Map.lookup str m of
		Just a -> return a
		Nothing -> Fail $ "Missing argument " ++ str

	grabOne :: String -> Map.Map String [String] -> Result String ;
	grabOne str m = case Map.lookup str m of
		Just a -> case a of
			(a:[]) -> return a
			_ -> Fail $ "One argument expected for " ++ str
		Nothing -> Fail $ "Missing argument " ++ str

	getByteString :: PacketType -> Map.Map String [String] -> Result BS.ByteString
	getByteString typ@(PingPacketType) _ =
		return $ S.encode $ (PacketResult (PacketHeader typ 0) (Success PingPacket))
			
	getByteString typ@(PingAckType) _ =
		return $ S.encode $ (PacketResult (PacketHeader typ 0) (Success PingAck))

	getByteString typ@(ReqConnectType) m = do
		from <- grabOne "from" m
		return $ S.encode $ (PacketResult (PacketHeader typ 0) (Success $ ReqConnect from ))
	
	getByteString typ@BroadcastPacketType m = do
		from <- grabOne "from" m
		to <- grab "to" m
		dat <- grab "data" m
		return $ S.encode $ (PacketResult (PacketHeader typ 0)
			(Success $ BroadcastPacket from to (fromString . concat $ dat)))

	getByteString _ _ = Fail "Not Implemented"
		

			

	main =
		let packet = (BroadcastPacket "josh" ["zach", "nic"] (fromString "hello")) in
		let bytestr = S.encode (PacketResult (PacketHeader BroadcastPacketType 0) (Success packet)) in

		getArgs >>= ( \args ->
			let maybeByteStr = 
				let argmap = parseArgs args in do
				packetType <- packetTypeFromMap argmap
				getByteString packetType argmap
			in
			case maybeByteStr of
				Success bytestr -> do
					BS.putStr $ runPut (putWord32be . fromIntegral $ BS.length bytestr)
					BS.putStr bytestr
				Fail reason -> hPutStrLn stderr $ "Fail: " ++ reason )
