module NetLion.Packets.Serialize where
	import NetLion.Common
	import NetLion.Packets
	import NetLion.Packets.Writer
	import NetLion.Packets.Parser

	import System.IO
	import Debug.Trace

	import qualified Data.Serialize as S
	import qualified Data.ByteString as BS
	import qualified Data.ByteString.Lazy as BSL

	__2byteheader :: BS.ByteString
	__2byteheader = BS.pack [0xAF,0xFF]

	instance S.Serialize PacketResult where
		put (PacketResult _ (Success packet)) =
			serializePacketPut packet

		put (PacketResult header (Fail str)) =
			serializePacketFail header str
			
		get = parsePacketGet 
	
	writePacket :: Handle -> Packet -> IO ()
	writePacket handle packet  = do
		let bytes = S.encode (packetToPacketResult packet)
		BS.hPutStr handle $ __2byteheader
		BS.hPutStr handle $ S.runPut (S.putWord32be . fromIntegral $ BS.length bytes)
--		putStrLn $ "Writing bytes: " ++ (show bytes) ++ " length: " ++ (show $ BS.length bytes) 
		BS.hPutStr handle $ bytes
		hFlush handle
	
	readPacket :: Handle -> IO PacketResult
	readPacket handle = do
		header <- BS.hGetSome handle 2
		trace "read two byte header" $
			if header /= __2byteheader then error $ "invalid 2 byte header! Got " ++ (show header)
			else do
				-- todo, BAD functional code!!!
				eithersize <- (BS.hGetSome handle 4) >>= return . (S.runGet S.getWord32be)
				case eithersize of
					Right size -> do
						--putStrLn $ "Tring to read " ++ (show size) ++ " bytes."
						chunk <- BS.hGetSome handle (fromIntegral size)
						--putStrLn $ "Read chunk: " ++ (show chunk)
						case (S.decode chunk) of
							Right pr -> return pr
							Left err -> error err
					Left err -> error err
