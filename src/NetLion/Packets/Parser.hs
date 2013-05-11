{- The Goal of this module is to provide useful functions
	for parsing ByteStrings into readable packets for
	NetLion -}
module NetLion.Packets.Parser where
	import NetLion.Server
	import NetLion.Common as C
--	import NetLion.Server.Actions
	import NetLion.Packets
	import Debug.Trace

	import qualified Data.Char as Char
	import qualified Data.ByteString.Lazy as BSL
	import qualified Data.ByteString as BS

	import Data.Serialize.Get

	{- Converst a byte string to an actual string,
		however if the string contains non-alphanumeric
		characters, it will fail. -}
	byteStringToLegitStr :: BSL.ByteString -> C.Result String
	byteStringToLegitStr =


		-- To check to see if it's an identifier	
		let isIdentifierChar = Char.isAlphaNum . Char.chr . fromIntegral in
		let asChar = Char.chr . fromIntegral in

		-- the fold function to run on the string
		let toFold = foldr
			(\ele ac -> 
				ac >>= (\str ->
					if (isIdentifierChar ele) then
						Success $ (asChar ele) : str
					else C.Fail $ "Invalid character in identifier: " ++ (show ele) ) ) (Success "")
		in

		toFold . BSL.unpack


	{- Parses a packet from a ByteString
		By using the `Get' monad -}
	parsePacketGet :: Get PacketResult
	parsePacketGet =
		{- Reads the header of the Packet
		The header simply contains two values; type which
		specifies the packet type and a status. For a successful
		packet, the status should be set to 0 -}
		let readHeader = do

			{- read the packet type from the ByteString,
				and convert it to the appropriate integer -}
			packetType <- getWord8 >>= return . toEnum . fromIntegral

			{- Similarly, retrieve the status from
				the ByteString and convert it -}
			status <- getWord8 >>= return . fromIntegral


			{- return the packetheader with the type
				and the status read -}
			return $ PacketHeader packetType status in

		{- Returns an identifier from a ByteString.
			That means. it will first read an integer that
			denotes the length of the string. Then it will read
			the string, failing if the string contains non-identifier
			characters. (AlphaNumeric) -}
		let getIdentifier :: Get (C.Result String) ; getIdentifier = do

			{- Read a byte string of the specified length.
				Using some magic Haskell black magic here! :-D -}
			(getWord32be >>= getLazyByteString . fromIntegral)
				>>= (return . byteStringToLegitStr) in

		{- Returns a list of identifiers. This function is
			used primarily to get a list of recipients from
			the broadcast message -}
		let getIdentifiers :: Get (C.Result [String]) ; getIdentifiers = do

			{- Read a binary blob. The format of this
				chunk of data is a 32 bit integer followed
				by a blob. The blob's size is the integer
				read previously

				Then we split by null terminating bytes -}
			lst <- getWord32be >>= getLazyByteString . fromIntegral
					>>= return . BSL.split 0

			{- Return a fold on the list that returns a successful list
				if all id's are apropriate identifiers -}
			return $ foldr (\bs acc ->

				if BSL.null bs then acc else

				acc >>= (\l ->
					(byteStringToLegitStr bs) >>= (\str -> return $ (str : l))
				)) (Success []) lst in


		{- Reads a packet given a certain type of
			Packet. (0 = PingPacket, 1 = PingAck etc) -}
		let readPacketFromType :: PacketType -> Get (C.Result Packet) ; readPacketFromType typ = 
			case typ of
				{- For the case of a PingPacket,
					this is all we need, so we can just
					return here -}
				-- todo replace these guys with enumerations
				PingPacketType -> return $ Success PingPacket

				{- For a PingAck, this is
					all the information needed as well -}
				PingAckType -> return $ Success PingAck

				{- For the ReqConnect packet, there is a little
					more information needed. We need to grab an identifier -}
				ReqConnectType -> getIdentifier >>=
						(\id -> return $ id >>= Success . ReqConnect)

				{- For a ReqConnectAck packet, all there is
					to grab is a 32-bit integer. -}
				ReqConnectAckType -> getWord32be >>= return . Success . ReqConnectAck . fromIntegral

				{- So far untested, but should theoretically, parse a packet
					with a client id and list of recipiants. -}
				BroadcastPacketType -> do
						{- Get the sender of the byte sequence -}
						from <- getIdentifier 

						{- Get the recipiants -}
						to <- getIdentifiers

						{- Get the bytes to send -}
						bytes <- getWord32be >>= getByteString . fromIntegral

						{- We have to match to make sure both from
							and to are valid arguments -}
						case (from, to) of
							(C.Fail s, _) -> return (C.Fail s)
							(_, C.Fail s) -> return (C.Fail s)

							{- If every thing worked well, we can return a
								successful BroadcastPacket -}
							(Success frm, Success t) -> return $ Success $ BroadcastPacket frm t bytes

				DataPacketType -> do
					{- get who the data is from -}
					from <- getIdentifier
						
					{- get the data from the packet -}
					bytes <- getWord32be >>= getByteString . fromIntegral

					case from of
						C.Fail s -> return (C.Fail s)
						Success frm -> return $ Success $ DataPacket frm bytes

				ReadPacketType -> do
					from <- getIdentifier
					follow <- getWord8 >>= return . (/=0)

					case from of
						C.Fail s -> return (C.Fail s)
						Success "" -> return $ Success $ ReadPacket Nothing follow
						Success str -> return $ Success $ ReadPacket (Just str) follow

				WritePacketType -> do
					{- Get the list of people to send the information
						to -}
					to <- getIdentifiers
					
					case to of
						{- Make sure the identifiers are valid -}
						C.Fail s -> return $ C.Fail s
						Success lst -> return $ Success $ WritePacket lst

				{- If something else, return a not implemented
					error -}
				_ -> return $ C.Fail "Not implemented" in

			do

			{- Here is where the real function starts.
				We just need to read the header -}
			header <- readHeader

			{- We just need to check the status code
				to make sure it's 0, or else fail -}
			case header of
				(PacketHeader typ 0) ->
					{- Deconstruct and return the new packet -}
					(readPacketFromType typ) >>= return . (PacketResult header)
				(PacketHeader typ s) -> return $ PacketResult header (C.Fail $ "Non-zero status: " ++ (show s))


	packetFromByteString :: BS.ByteString -> C.Result Packet
	packetFromByteString datagram =
		{- Run the Get to grab the packet
			from the ByteString -}
		case (runGet parsePacketGet datagram) of
			Right (PacketResult header packet) -> packet
			Left str -> C.Fail str
