module NetLion.Server where
	import Data.Map as Map
	import Data.ByteString as BS
	import Control.Concurrent
	import Control.DeepSeq
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
	initServer socket portno = do
		newEmptyMVar >>= (\mvar ->
			return (Server Map.empty mvar
				(ServerConnectionData socket portno) ) )

	-- Add a client to the server.
	serverAddClient :: Client -> Server -> Server
	serverAddClient client (Server map mvar cd) =
		(Server (Map.insert (getClientId client) client map) mvar cd)

	serverRemoveClient :: ClientId -> Server -> Server
	serverRemoveClient client (Server map mvar cd) = 
		(Server (Map.delete client map) mvar cd)

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
	serverGetClientById id (Server map _ _) =
		Map.lookup id map
