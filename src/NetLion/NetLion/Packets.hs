module NetLion.Packets where
	import NetLion.Server
	import NetLion.Common

	type PacketStatus = Int
	-- type and status
	data PacketHeader = PacketHeader PacketType PacketStatus deriving Show
	-- a result, contains a packet header and
	-- either a Packet or a reasong for failure
	data PacketResult = PacketResult PacketHeader (Result Packet) deriving Show

	-- The different types of actions
	-- that can exist
	data Packet =
		PingPacket |
		PingAck    |

		ReqConnect ClientId |
		ReqConnectAck Int |

		BroadcastPacket ClientId [ClientId] Datagram |
		BroadcastPacketAck |

		Logout ClientId |
		LogoutAck | 
		DataPacket ClientId Datagram |

		-- these guys should probably be separate since they
		-- are a part of the intermediate protocol
		ReadPacket (Maybe ClientId) Bool |
		WritePacket [ClientId] deriving Show
	
	
	data PacketType =
		PingPacketType |
		PingAckType |
		ReqConnectType |
		ReqConnectAckType |
		BroadcastPacketType |
		BroadcastPacketAckType |
		LogoutType |
		LogoutAckType |
		DataPacketType |
		ReadPacketType |
		WritePacketType  
		deriving Show
	
	instance Enum PacketType where
		fromEnum PingPacketType				= 0
		fromEnum PingAckType				= 1
		fromEnum ReqConnectType				= 2
		fromEnum ReqConnectAckType			= 3
		fromEnum BroadcastPacketType 		= 4
		fromEnum BroadcastPacketAckType 	= 5
		fromEnum LogoutType 				= 6
		fromEnum LogoutAckType 				= 7
		fromEnum DataPacketType 			= 8
		fromEnum ReadPacketType 			= 9
		fromEnum WritePacketType 			= 10

		toEnum val =
			case (val `mod` 11) of
				0  -> PingPacketType
				1  -> PingAckType
				2  -> ReqConnectType
				3  -> ReqConnectAckType
				4  -> BroadcastPacketType
				5  -> BroadcastPacketAckType
				6  -> LogoutType
				7  -> LogoutAckType
				8  -> DataPacketType
				9  -> ReadPacketType
				10 -> WritePacketType
				-- it would make no sense mathematically to
				-- get to this point
				_ -> PingPacketType

		succ             =  toEnum . (+1) . fromEnum
		pred             =  toEnum . (subtract 1) . fromEnum
		enumFrom x       =  map toEnum [fromEnum x ..]
		enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
		enumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]

	{- Returns the type of a packet given
		the packet itself.
		
		The type is an enumeration which can be
		easily converted between an integer
		and enum  -}
	getPacketType :: Packet -> PacketType
	getPacketType PingPacket = PingPacketType
	getPacketType PingAck = PingAckType

	getPacketType (ReqConnect _) = ReqConnectType
	getPacketType (ReqConnectAck  _ ) = ReqConnectAckType

	getPacketType (BroadcastPacket  _ _ _ ) = BroadcastPacketType
	getPacketType BroadcastPacketAck = BroadcastPacketAckType

	getPacketType (Logout  _) = LogoutType
	getPacketType LogoutAck = LogoutAckType

	getPacketType (DataPacket _ _) = DataPacketType
	getPacketType (ReadPacket _ _) = ReadPacketType
	getPacketType (WritePacket _) = WritePacketType

	packetToPacketResult :: Packet -> PacketResult
	packetToPacketResult packet =
		(PacketResult (PacketHeader (getPacketType packet) 0) (Success packet) )
