{-# LANGUAGE OverloadedStrings #-}

module Reverseme where

import Data.Aeson
import System.Directory
import System.Posix.Files
import System.FilePath.Posix
import System.Cmd (rawSystem)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException, handle)
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS

data CInterface = CInterface {
	ciIp :: String,
	ciPort :: Int
} deriving (Eq)

instance ToJSON CInterface where
	toJSON (CInterface i p) = object ["ip" .= i, "port" .= p]

instance FromJSON CInterface where
	parseJSON (Object v) = 
		CInterface <$> 
		v .: "ip"
		<*> v .: "port"

instance Show CInterface where
	show (CInterface h p) = h ++ ":" ++ show p::String

data Container = Container {
	cName :: String,
	cInterface :: CInterface
} deriving (Eq)

instance ToJSON Container where
	toJSON (Container n i) = object ["name" .= n, "interface" .= i]

instance FromJSON Container where
	parseJSON (Object v) = 
		Container <$> 
		v .: "name"
		<*> v .: "interface"

instance Show Container where
	show (Container n i) = show i

data NginxVhost = NginxVhost {
	vhAuthor :: String,
	vhName :: String,
	vhType :: String,
	vhContainers :: [Container]
} deriving (Eq)

instance ToJSON NginxVhost where
	toJSON (NginxVhost a n t c) = object [
		"author" .= a,
		"name" .= n,
		"type" .= t,
		"containers" .= c]

instance FromJSON NginxVhost where
	parseJSON (Object v) = 
		NginxVhost <$> 
		v .: "author"
		<*> v .: "name"
		<*> v .: "type"
		<*> v .: "containers"

instance Show NginxVhost where
	show (NginxVhost a n t c) = unlines ["# Author: " ++ a ++ "\n",
		"upstream " ++ vhname ++ "_backend {",
		unlines (map (\x -> "\tserver " ++ show x ++ " max_fails=3 fail_timeout=30s;") c), -- crado voir fmap
		"}\n\nserver {",
		"\tlocation /" ++ vhname ++ "{",
		"\t\tproxy_set_header Host $host;",
		"\t\tproxy_set_header X-Real-IP $remote_addr;",
		"\t\tproxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;",
		"\t\trewrite /(.*) /$1 break;",
		"\t\tproxy_pass http://" ++ vhname ++ "_backend;",
		"\t}\n}"]
		where
			vhname = n ++ "-" ++ t

data NginxConfiguration = NginxConfiguration {
	ngcVHostPath :: FilePath,
	ngcInetdBinPath :: String
} deriving (Eq, Show)

-- rawSystem (read (ngcInetdBinPath c)) ["reload"]
ngxReload c = liftIO $ rawSystem "service" ["nginx","reload"] >> return ()

ngxGetDirectory :: NginxVhost -> String
ngxGetDirectory v = vhName v ++ "-" ++ vhType v

ngxWriteVHost :: NginxConfiguration -> NginxVhost -> IO ()
ngxWriteVHost c v = writeFile vhFilePath $ show v
	where
		vhFilePath = ngcVHostPath c </> ngxGetDirectory v

ngxDropVHost :: NginxConfiguration -> NginxVhost -> IO ()
ngxDropVHost c v = removeFile vhFilePath
	where
		vhFileName = vhName v ++ "-" ++ vhType v
		vhFilePath = ngcVHostPath c </> vhFileName

exceptionMain :: SomeException -> IO()
exceptionMain ex = putStrLn $ "Ya comme une merde ! : " ++ show ex

{--
main :: IO ()
main = handle exceptionMain $ do
	putStrLn "# ---\n# vhost auto configuration\n#---"
	putStr "# json : "
	BS.putStrLn $ encode nginxVhost
	--C.putStrLn $ encode nginxVhostFailed
	putStrLn "# ---"
	putStrLn $ show nginxVhost
	--putStrLn $ show nginxVhostFailed
	putStrLn "# --- end auto configuration"
	putStrLn "# write file conf"
	ngxWriteVHost nginxConfig nginxVhost
	--ngxWriteVHost nginxConfig nginxVhostFailed
	putStrLn "# reload nginx"
	ngxReload nginxConfig
	putStrLn "# ---"
	where
		nginxVhost = NginxVhost "111117" "gsp2-2.1.4" "dev" [(Container "gsp2" (CInterface "127.0.0.1" 8080))]
		nginxConfig = NginxConfiguration "/tmp/" "/etc/init.d/nginx"
		(Just nginxVhostFailed) = decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe NginxVhost
		--}
