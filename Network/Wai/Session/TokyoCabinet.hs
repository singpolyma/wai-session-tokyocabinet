module Network.Wai.Session.TokyoCabinet (tokyocabinetStore, tokyocabinetStore_) where

import Control.Monad
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Network.Wai.Session (Session, SessionStore, genSessionId)
import Control.Error (hush)
import qualified Data.ByteString as BS

import Database.TokyoCabinet
import Data.Serialize (encode, decode, Serialize)

-- | Session store that keeps all content in TokyoCabinet
--
-- WARNING: This session is vulnerable to sidejacking,
-- use with TLS for security.
tokyocabinetStore :: (TCDB a, Serialize k, Serialize v, MonadIO m) =>
	IO ByteString
	-- ^ 'IO' action to generate unique session IDs
	-> a
	-- ^ TokyoCabinet handle
	-> SessionStore m k v
tokyocabinetStore _ db (Just sk) = backend db sk
tokyocabinetStore genNewKey db Nothing = genNewKey >>= backend db

-- | Store using simple session ID generator based on time and 'Data.Unique'
tokyocabinetStore_ :: (TCDB a, Serialize k, Serialize v, MonadIO m) => a -> SessionStore m k v
tokyocabinetStore_ = tokyocabinetStore genSessionId

backend :: (TCDB a, Serialize k, Serialize v, MonadIO m) => a -> ByteString -> IO (Session m k v, IO ByteString)
backend db sk =
	return ((
			liftTCM . liftM (join . liftM (hush . decode)) . get db . fullKey,
			(\k v -> liftTCM $ put db (fullKey k) (encode v) >> return ())
		), return sk)
	where
	liftTCM = liftIO . runTCM
	fullKey k = sk `BS.append` encode k
