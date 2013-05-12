module NetLion.Server where
	import Data.Map as Map
	import Data.ByteString as BS
	import Control.Concurrent
	import System.IO
	import Network.Socket

	type ClientId = String

	type Datagram = BS.ByteString

	data Client = Client {
		getClientHandle :: Handle,
		getClientId :: ClientId
	}

	data ServerConnectionData = ServerConnectionData Socket PortNumber
	data Server = Server (Map.Map ClientId Client) (MVar (Server -> IO Server)) ServerConnectionData

	initServer :: Socket -> PortNumber -> IO Server
	initServer sock portno = do
		newEmptyMVar >>= (\mvar ->
			return (Server Map.empty mvar
				(ServerConnectionData sock portno) ) )

	-- Add a client to the server.
	serverAddClient :: Client -> Server -> Server
	serverAddClient client (Server m mvar cd) =
		(Server (Map.insert (getClientId client) client m) mvar cd)

	serverRemoveClient :: ClientId -> Server -> Server
	serverRemoveClient client (Server m mvar cd) = 
		(Server (Map.delete client m) mvar cd)

	-- Queue an action on the server's MVar
	serverQueueAction :: (Server -> IO Server) -> Server -> IO ()
	serverQueueAction func (Server _ mvar _) =	
		putMVar mvar func

	-- Take an action off the queue of this server's
	serverTakeAction :: Server -> IO (Server -> IO Server)
	serverTakeAction (Server _ mvar _ ) =
		takeMVar mvar

	-- Runs a server
	runServer :: Server -> IO Server
	runServer server =
		(serverTakeAction server) >>= ( \func ->
			(func server) >>= runServer
		)

	-- Gets a client from this server by the ClientId
	serverGetClientById :: ClientId -> Server -> Maybe Client
	serverGetClientById clid (Server m _ _) =
		Map.lookup clid m
