{-# LANGUAGE OverloadedStrings #-}

module Reverseme where

import Data.Aeson
import System.IO
import System.Posix.Files
import Control.Exception
import System.Cmd (rawSystem)
-- import Data.Map (Map)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
-- import qualified Data.ByteString.Char8 as BS

{--
	λ> :load  reverseme.hs
	[1 of 1] Compiling Main             ( reverseme.hs, interpreted )
	Ok, modules loaded: Main.
	λ> let (Just r) = decode (BS.pack jsVHost) :: Maybe Vhost
	λ> r
	Vhost {name = "gsp2-prod", containers = [Container (fromList [("10.226.150.12","9001")]),Container (fromList [("10.226.150.12","9002")]),Container (fromList [("10.226.150.12","9003")])]}

	{"name":"google-v1.0","env":"prod","user":"111117","containers":[{"ip":"10.226.150.12","port":9001},{"ip":"173.194.40.120","port":80}]}
	http://127.0.0.1:8000/append/{"name":"google-v1.0","env":"prod","user":"111117","containers":[{"ip":"10.226.150.12","port":9001},{"ip":"173.194.40.120","port":80}]}
--}
jsVHost :: String
jsVHost = "{\"name\":\"google-v1.0\",\"env\":\"prod\",\"user\":\"111117\",\"containers\":[{\"ip\":\"10.226.150.12\",\"port\":9001}, {\"ip\":\"173.194.40.120\",\"port\":80}]}"

data Deploy = Deploy {
	vhost :: String,
	message :: String
} deriving Show	

data Container = Container {
	ip :: String, 
	port :: Int
} deriving Show

data Vhost = Vhost {
	name :: String,
	env :: String,
	user :: String,
	containers :: [Container]
} deriving Show

instance FromJSON Container where
	parseJSON (Object v) = 
		Container <$>
		v .: "ip"
		<*> v .: "port"

instance FromJSON Vhost where
	parseJSON (Object v) = 
		Vhost <$> 
		v .: "name"
		<*> v .: "env"
		<*> v .: "user"
		<*> v .: "containers" 

{--------------------------------------------------------------------------------------------------------------}
writeNginxConf :: String -> IO ()
writeNginxConf json = do
	nginxSiteEnableFS <- safeFileStatus nginxConfigDir
	nginxVHostFS <- safeFileStatus vhFileName
	case nginxSiteEnableFS of
		Just x -> if isDirectory x then
				case nginxVHostFS of
					Just x -> liftIO $ writeFile "/tmp/log" "error"
					Nothing -> do 
						liftIO $ writeFile vhFileName (makeNginxVhost vh)
						liftIO $ writeFile "/tmp/log" "config ok ..."
						liftIO $ rawSystem "/etc/init.d/nginx" ["reload"] >> return ()
			else
				liftIO $ writeFile "/tmp/log" "error"
		Nothing -> liftIO $ writeFile "/tmp/log" "error"
	--putStrLn $ makeNginxVhost vh
	liftIO $ writeFile "/tmp/log-rme.txt" $ makeNginxVhost vh
	where
		(Just vh) = decode (BSL.pack json) :: Maybe Vhost
		nginxConfigDir = "/etc/nginx/sites-enabled/" 
		vhFileName = nginxConfigDir ++ name vh ++ "-" ++ env vh

safeFileStatus :: System.IO.FilePath -> IO (Maybe FileStatus)
safeFileStatus path = handle errorHandler $ fmap Just $ getFileStatus path
	where
	errorHandler :: SomeException -> IO (Maybe FileStatus)
	errorHandler _ = return Nothing

makeNginxVhost :: Vhost -> [Char]
makeNginxVhost vh = unlines ["# author: " ++ user vh,
		"upstream " ++ vhname ++ "_backend {",
		unlines (map (\x -> "\tserver " ++ ip x ++ ":" ++ show (port x) ++ " max_fails=3 fail_timeout=30s;") $ containers vh) ++ "}",
		"server {",
		"\tlocation /" ++ vhname ++ "{",
    		"\t\tproxy_set_header Host $host;",
    		"\t\tproxy_set_header X-Real-IP $remote_addr;",
    		"\t\tproxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;",
		"\t\trewrite /(.*) /$1 break;",
		"\t\tproxy_pass http://" ++ vhname ++ "_backend;",
		"\t}\n}"]
		where 
			vhname = name vh ++ "-" ++ env vh