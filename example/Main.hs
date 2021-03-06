module Main where

import Data.Default (def)
import Data.String (fromString)
import qualified Data.Vault as Vault

import Network.Wai
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.TokyoCabinet (tokyocabinetStore_)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (ok200)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Database.TokyoCabinet as TC

app :: Vault.Key (Session (ResourceT IO) String String) -> Application
app session env = do
	u <- sessionLookup "u"
	sessionInsert "u" insertThis
	return $ responseLBS ok200 [] $ maybe (fromString "Nothing") fromString u
	where
	insertThis = show $ pathInfo env
	Just (sessionLookup, sessionInsert) = Vault.lookup session (vault env)

openTokyoCabinet :: (TC.TCDB a) => FilePath -> IO a
openTokyoCabinet pth = TC.runTCM $ do
	db <- TC.new
	True <- TC.open db pth [TC.OREADER, TC.OWRITER, TC.OCREAT]
	return db

main :: IO ()
main = do
	session <- Vault.newKey
	store <- fmap tokyocabinetStore_ (openTokyoCabinet "./session.tcdb" :: IO TC.HDB)
	run 3000 $ withSession store (fromString "SESSION") def session $ app session
