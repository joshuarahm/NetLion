module NetLion.Packets.Serialize where
	import NetLion.Common
	import NetLion.Packets
	import NetLion.Packets.Writer
	import NetLion.Packets.Parser

	import qualified Data.Serialize as S

	instance S.Serialize PacketResult where
		put (PacketResult _ (Success packet)) =
			serializePacketPut packet

		put (PacketResult header (Fail str)) =
			serializePacketFail header str
			

		get = parsePacketGet 
