module NetLion.Packets.Serialize where
	import NetLion.Common
	import NetLion.Packets
	import NetLion.Packets.Writer
	import NetLion.Packets.Parser

	import System.IO
	import qualified Data.Serialize as S
	import qualified Data.ByteString as BS

	__2byteheader :: BS.ByteString
	__2byteheader = BS.pack [0xAF,0xFF]

	{- Create an instance of Packets being
		Serializable -}
	instance S.Serialize PacketResult where
		put (PacketResult _ (Success packet)) =
			serializePacketPut packet

		put (PacketResult header (Fail str)) =
			serializePacketFail header str
			
		get = parsePacketGet 
	
	{- Writes a packet to a handle -}
	writePacket :: Handle -> Packet -> IO ()
	writePacket handle packet  = do
		{- encode the bytes from a resulting packet -}
		let bytes = S.encode (packetToPacketResult packet)

		{- send the mandatory two byte header -}
		BS.hPutStr handle $ __2byteheader

		{- Write the length of the rest of the message
			in the header -}
		BS.hPutStr handle $ S.runPut (S.putWord32be . fromIntegral $ BS.length bytes)
--		putStrLn $ "Writing bytes: " ++ (show bytes) ++ " length: " ++ (show $ BS.length bytes) 
		BS.hPutStr handle $ bytes

		-- flush for good measure
		hFlush handle
	
	readPacket :: Handle -> IO PacketResult
	{- Reads a packet from a handle and returns
		the packet result -}
	readPacket handle = do
		{- read the first two bytes. By convention, these
			first two bytes must be 0xAF and 0xFF in that order. -}
		header <- BS.hGetSome handle 2
		{- we will want to check the header with the
			two bytes -}
		if header /= __2byteheader
			then do
				error $ "invalid 2 byte header! Got " ++ (show header)

			{- if the first two bytes match -}
			else do
				-- todo, BAD functional code!!!
				{- Read the next integer. This should be the length of the packet
					to read -}
				eithersize <- (BS.hGetSome handle 4) >>= return . (S.runGet S.getWord32be)
				case eithersize of
					{- If the size was correctly read. e.g. there was enough
						bytes to read from the stream -}
					Right size -> do
						{- Read the rest from the handle -}
						chunk <- BS.hGetSome handle (fromIntegral size)
	
						{- Try to decode the chunk of data just read -}
						case (S.decode chunk) of
							Right pr -> return pr
							Left err -> error err
	
					{- in case there was not enough information	
						left to read -}
					Left err -> error err
