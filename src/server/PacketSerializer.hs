module NetLion.Packets.Writer where
	import NetLion.Server
	import NetLion.Common

	import NetLion.Packets

	import Data.Serialize.Put

	import qualified Data.String as String
	import qualified Data.ByteString.Lazy as BSL
	import qualified Data.ByteString as BS

	{- This function serializes a packet
		by returning a ByteString version of it -}
	serializePacketSuccess :: Packet -> BS.ByteString
	serializePacketSuccess packet =  
		runPut (serializePacketPut packet)
		
	{- Serializes a failed packet to a bytes string -}
	serializePacketFail :: PacketHeader -> String -> Put
	serializePacketFail header reason = do
		{- Serialize the header. The header includes the
			type and exit code -}
		serializePacketHeader header

		{- Serialize the size of the reason string
			and follow it with the actual string -}
		putWord32be . fromIntegral $ length reason
		putByteString . String.fromString $ reason 

	{- Puts a header for a packet -}
	serializePacketHeader :: PacketHeader -> Put
	serializePacketHeader (PacketHeader typ stat) = do
		putWord8 . fromIntegral . fromEnum $ typ
		putWord8 . fromIntegral $ stat
	
	serializeString :: String -> Put
	serializeString str = do
		putWord32be . fromIntegral $ length str
		putByteString . String.fromString $ str


	serializePacketPut :: Packet -> Put
	{- All it takes to serialize a PingAck is to
		serialize the header -}
	serializePacketPut PingAck = 
		serializePacketHeader (PacketHeader PingAckType 0)
	
	{- All it takes to serialize a PingPacket is 
		to serialize the header -}
	serializePacketPut PingPacket = 
		serializePacketHeader (PacketHeader PingPacketType 0)

	{- All it takes to serialize a LogoutAck is to
		serialize the header -}
	serializePacketPut LogoutAck = 
		serializePacketHeader (PacketHeader LogoutAckType 0)

	{- All it takes to serialize a BroadcastPacketAck
		is to serialize the header -}
	serializePacketPut BroadcastPacketAck = 
		serializePacketHeader (PacketHeader BroadcastPacketAckType 0)
	

	{- Below are all the non-trivial packets to send -}
	serializePacketPut (ReqConnect clientId) = do
		serializePacketHeader (PacketHeader ReqConnectType 0)
		{- serialize the client id -}
		serializeString clientId

	serializePacketPut (ReqConnectAck clid ) = do
		serializePacketHeader (PacketHeader ReqConnectAckType 0)
		putWord32be . fromIntegral $ clid

	{- This is the hard one -}
	serializePacketPut (BroadcastPacket from to bs) = do
		let serializeClientList lst =
			putByteString $ BS.concat . map ( ((flip BS.snoc) 0) . String.fromString ) $ lst

		serializePacketHeader (PacketHeader BroadcastPacketType 0)
		serializeString from
		serialList <- (return $ runPut $ serializeClientList to)
		putWord32be . fromIntegral $ BS.length serialList
		putByteString serialList
		putWord32be . fromIntegral $ BS.length bs
		putByteString bs
		
	serializePacketPut (Logout clientId) = do
		serializePacketHeader (PacketHeader LogoutType 0)
		serializeString clientId
	
	serializePacketPut (DataPacket from bs) = do
		serializePacketHeader (PacketHeader DataPacketType 0)

		serializeString from
		putWord32be . fromIntegral $ BS.length bs
		putByteString bs




