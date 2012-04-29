{-# LANGUAGE OverloadedStrings #-}

module Main where 

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Text.XmlHtml
import Control.Monad.Writer
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString.Lazy (toChunks)
import Data.List
import Data.Maybe
import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)
import qualified Network.HTTP.Types as HT
import Network.HTTP.Conduit
                                             
--------------------------------------------------
-- data types

data OpenID2 = OpenID2 { openidIdentify :: BS.ByteString
                       , openidMode     :: BS.ByteString      -- [ "checkid_setup", "checkid_immediate" ]
                       , openidReturnto :: BS.ByteString
                       }

gopenid :: OpenID2
gopenid = OpenID2 { openidIdentify = "https://www.google.com/accounts/o8/id"
                  , openidMode     = "checkid_setup"
                  , openidReturnto = "http://0.0.0.0:8800/gcallback"
                  }

requestEndpointParam :: HT.SimpleQuery
requestEndpointParam = [ ("openid.ns" ,"http://specs.openid.net/auth/2.0")
                       , ("openid.claimed_id", "http://specs.openid.net/auth/2.0/identifier_select")
                       , ("openid.identity","http://specs.openid.net/auth/2.0/identifier_select")
                       ]
                       
xrdsAccept :: HT.RequestHeaders
xrdsAccept = [("Accept", "application/xrds+xml")]
  
--------------------------------------------------
-- Request UTIL

insertHeader :: HT.RequestHeaders -> Request a -> Request a
insertHeader hs r = r { requestHeaders = requestHeaders r ++ hs }

toReq' :: BS.ByteString  -- ^ A request string
       -> Request a
toReq' = fromJust . parseUrl . BS.unpack

doRequest :: Request IO -> IO (Response BSL.ByteString)
doRequest = withManager . httpLbs

appendQueryString :: BS.ByteString    -- ^ a Request URL
               -> HT.SimpleQuery   -- ^ query parameters
               -> BS.ByteString
appendQueryString url [] = url
appendQueryString url xs = url `BS.append` (renderSimpleQuery True xs)

-- | FIXME: withGet and withPost are a little duplicated

withRequestGet :: HT.RequestHeaders   -- ^ Request Headers
               -> BS.ByteString       -- ^ Request query string, including parameters
               -> IO (Response BSL.ByteString)
withRequestGet [] url = doRequest $ toReq' url
withRequestGet hs url = doRequest $ insertHeader hs $ toReq' url

withRequestPost :: HT.RequestHeaders      -- ^ Request Headers
                    -> BS.ByteString       -- ^ Request query string, including parameters
                    -> [(BS.ByteString, BS.ByteString)]       -- ^ post body
                    -> IO (Response BSL.ByteString)
withRequestPost [] url body = doRequest $ urlEncodedBody body $ toReq' url                   
withRequestPost hs url body = doRequest $ insertHeader hs $ urlEncodedBody body $ toReq' url

--------------------------------------------------
-- Access Google OpenID 

requestIdentity :: OpenID2 -> IO (Response BSL.ByteString)
requestIdentity o = withRequestGet xrdsAccept $ (openidIdentify o) `appendQueryString` []

getEndpointURL :: OpenID2 -> BS.ByteString -> BS.ByteString
getEndpointURL o url = url 
                       `appendQueryString` requestEndpointParam 
                       `appendQueryString` [ ("openid.mode", openidMode o)
                                           , ("openid.return_to", openidReturnto o)
                                           , ("openid.realm", openidReturnto o)]

main :: IO ()
main = do
       uri <- liftM (E.encodeUtf8 . findURI . responseBody) (requestIdentity gopenid)
       print $ getEndpointURL gopenid uri
       
       -- ^ browse this URL manually
       -- will navigate to google; user conform auth and navigate back to the `return_to` URL.
       -- FIXME: what's next ??? no comphrensive doc found ??
       --       http://openid.net/specs/openid-authentication-2_0.html


--------------------------------------------------
-- XML Response Utils

-- | Find the Endpoint value from openid identity request response. 
--  FIXME: return Either ???
findURI :: BSL.ByteString -> T.Text
findURI res = either (T.pack . (++ "Getting openID endpoint error: Can not find URI value"))
                     (findTag "URI" . docContent) 
                     (parseXML "parsexml.log" (bslToBS' res))

-- | Find a tag value among nodes.
--  FIXME: is it possible to use <$>/<*> to be point free function ???
findTag :: T.Text -> [Node] -> T.Text
findTag t = foldr (T.append . nodeText)  T.empty . concatMap (descendantElementsTag t)

-- | Convert Lazy.ByteString to ByteString.
bslToBS' :: BSL.ByteString -> BS.ByteString
bslToBS' = foldr BS.append BS.empty . toChunks

-- | Test Data
testtxt = BSL.pack "<xrds:XRDS> <XRD> <Service> <Type>http://specs.openid.net/auth/2.0/server</Type> <Type>http://openid.net/srv/ax/1.0</Type> <Type>http://specs.openid.net/extensions/ui/1.0/mode/popup</Type> <Type>http://specs.openid.net/extensions/ui/1.0/icon</Type> <Type>http://specs.openid.net/extensions/pape/1.0</Type> <URI>https://www.google.com/accounts/o8/ud</URI> </Service> </XRD> </xrds:XRDS>"



