module NetLion.Common where
	import qualified Data.ByteString as BS
	import Data.Char


	-- Function which takes a list and applies a monadic operation
	forAll :: Monad m => [t] -> b -> (t -> m a) -> m b
	forAll (x:xs) d f = (f x) >>= (\_ -> forAll xs d f)
	forAll [] def _ = return def

	forAllWithLast :: Monad m => [t] -> a -> (a -> b) -> (t -> a -> m a) -> m b
	forAllWithLast (x:xs) fst def f = (f x fst) >>= (\ l -> forAllWithLast xs l def f )
	forAllWithLast [] l def _ = return (def l)
	
	type FailureData = String
	data Result a = Success a | Fail FailureData deriving Show

	instance Monad Result where
		(Success x) >>= func = func x
		(Fail x) >>= _ = (Fail x)
		return = Success

	byteStringToString :: BS.ByteString -> String
	byteStringToString bytestring =
		let unpack = BS.unpack bytestring in
			map (\e -> chr $ fromIntegral e) unpack
