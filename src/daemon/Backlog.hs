module Backlog (appendToBacklog,backlogGetChan,Backlog,newBacklog,Backlog(Backlog))
	where
	import Control.Concurrent.BoundedChan
	import Control.Concurrent.MVar

	import NetLion.Packets
	import NetLion.Packets.Serialize

	import qualified Data.Map as Map
	import qualified Data.ByteString as BS

	{- The maximum number of data packets to
		retain in a user's queue without
		blocking. This is set to a megabyte per
		user -}
	maxBacklog :: (Integral a) => a
	maxBacklog = 1024


	{- A mutable data structure that holds the queue's for
		each contact as well as a queue to post information
		to the server on and a user name to register to the
		server with -}
	data Backlog = Backlog
		{- A mutable variable that contains the mappings from
			the client names to the bounded channels -}
		(MVar ( Map.Map String (BoundedChan BS.ByteString )))

		{- A queue of packet writes to the server -}
		(BoundedChan Packet) String

	{- Creates a new Backlog with a username -}
	newBacklog :: String -> IO Backlog
	newBacklog name = do
		-- create new mutable variables
		mvar <- newMVar Map.empty
		-- this is for the server writes
		bc <- newBoundedChan maxBacklog
		return (Backlog mvar bc name)

	appendToBacklog :: String -> BS.ByteString -> Backlog -> IO ()

	{- append information to a backlog -}
	appendToBacklog clid dat (Backlog mvar _ _) =
		{- To append to the backlog, we need
			to modify the mutable variable for the Map -}
		modifyMVar_ mvar (\m ->
			{- Check to see if there is already
				a channel for the client id -}
			case Map.lookup clid m of
				Just chan -> do
					{- if there is, then we can
						just write to that channel -}
					writeChan chan dat
					return m
				Nothing -> do
					{- if not, then we have to explicitly
						go through and create a new channel
						and map it to that client id -}
					chan <- newBoundedChan maxBacklog
					writeChan chan dat
					return $ Map.insert clid chan m)

	-- returns a channel from the backlog mapped to the client id
	backlogGetChan :: String -> Backlog -> IO (Maybe (BoundedChan BS.ByteString))
	backlogGetChan clid (Backlog mvar _ _ ) =
		withMVar mvar (\clmap -> return (Map.lookup clid clmap) )
