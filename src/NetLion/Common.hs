module NetLion.Common where
	import qualified Data.ByteString as BS
	import Data.Char
	import Control.Monad
	import qualified Data.Map as Map
	import Debug.Trace
	import Data.List

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

	{- Does a quick conversion of ByteString to
		String -}
	byteStringToString :: BS.ByteString -> String
	byteStringToString bytestring =
		let unpack = BS.unpack bytestring in
			map (\e -> chr $ fromIntegral e) unpack

	{- Simply parses arguments from the command line by
		grouping arguments of a switch -}
	parseArgs :: [String] -> Map.Map String [String]
	parseArgs args =
		let spltlist = filter (not . null) $ splitList (isPrefixOf "-") args in
		trace ("spltlist=" ++ (show spltlist)) $
		foldl (\acc e ->
			case e of
				(a:as) -> Map.insert (tail a) as acc
				_ -> acc ) Map.empty spltlist

	-- -t bcast --from josh --to nic zach --data "Hello There People"
	-- -t connect --from josh
	splitList :: (a -> Bool) -> [a] -> [[a]]
	splitList func list =
		case break func list of
			(la,[]) -> [la]
			(la,(a:as)) -> case (splitList func as) of
				(b:bs) -> la : (a:b) : bs
				l -> (la ++ [a]) : l

	grabExists :: String -> Map.Map String [String] -> Result Bool
	grabExists str m = case Map.lookup str m of
		Just a -> if null a then
				Success $ True
				else Fail $ str ++ " takes no arguments!"
		_ -> return False

	grab :: String -> Map.Map String [String] -> Result [String] ;
	grab str m = case Map.lookup str m of
		Just a -> return a
		Nothing -> Fail $ "Missing argument " ++ str

	grabOne :: String -> Map.Map String [String] -> Result String ;
	grabOne str m = case Map.lookup str m of
		Just a -> case a of
			(a:[]) -> return a
			_ -> Fail $ "One argument expected for " ++ str
		Nothing -> Fail $ "Missing argument " ++ str

	grabOneAll :: Map.Map String [String] -> [String] -> Result [String]
	grabOneAll m = mapM (flip grabOne $ m)

	grabAll :: Map.Map String [String] -> [String] -> Result [[String]]
	grabAll m = mapM (flip grab $ m)

	grabOneWithDefaults :: Map.Map String [String] -> [(String, Maybe String)] -> Result [String]
	grabOneWithDefaults m = mapM (\(str,def) ->
		let str' = grabOne str m in
		case str' of
			Success s -> return s
			Fail s -> case def of
				Just jstr -> return jstr
				Nothing -> Fail s )
