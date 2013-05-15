module ChannelHandler where
	import Control.Concurrent.BoundedChan
	import System.IO
	import qualified Data.Serialize as S
	import Data.ByteString (ByteString)
	import qualified Data.ByteString as BS
	
	type ChannelHandler a = BoundedChan ByteString -> Handle -> a -> IO ()

	foreverRead :: ChannelHandler ()
	foreverRead chan handle _ = do
		iseof <- hIsEOF handle
		-- if eof, then done
		if iseof then return ()
		else do
			bs <- readChan chan
			BS.hPutStr handle bs
			foreverRead chan handle ()

	readUntilNull :: ChannelHandler ()
	readUntilNull chan handle _ = do
		iseof <- hIsEOF handle

		if iseof then return()
		else do
			bs <- readChan chan
			BS.hPutStr handle bs
			if BS.null bs then return ()
			else readUntilNull chan handle ()

	flushChannel :: ChannelHandler ()
	flushChannel chan handle _ = do
		iseof <- hIsEOF handle
		isempty <- isEmptyChan chan

		if iseof || isempty  then return ()
		else do
			bs <- readChan chan
			BS.hPutStr handle bs
			flushChannel chan handle ()

	readOne :: ChannelHandler ()
	readOne chan handle _ = do
		iseof <- hIsEOF handle
		if iseof then return ()
		else do
			readChan chan >>= BS.hPutStr handle
			return ()
