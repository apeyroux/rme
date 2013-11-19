{-# LANGUAGE OverloadedStrings #-}

module Reverseme where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as C

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
	toJSON (NginxVhost a n t c) = object ["author" .= a,
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

main :: IO ()
main = do
	putStrLn "---"
	putStrLn $ show nginxVhost
	putStrLn "---"
	C.putStrLn $ encode nginxVhost
	putStrLn "---"
	--C.putStrLn $ encode (Container "gsp2" (CInterface "127.0.0.1" 80))
	--C.putStrLn $ show (NginxVhost "111117" "gsp2-0.1" "Dev" [])
	where
		nginxVhost = NginxVhost "111117" "gsp2-0.1" "dev" [(Container "gsp2" (CInterface "127.0.0.1" 8080))]