{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.Webmachine where
import           Blaze.ByteString.Builder (toByteString)
import           Control.Category
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.RWS
import           Data.Aeson (ToJSON, FromJSON, encode, decode)
import           Data.Bool
import           Data.ByteString (ByteString, splitWith)
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.Conduit (Source)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import           Data.Maybe
import           Data.Either
import           Data.Text (Text)
import           Network.HTTP.Types
import           Network.Wai.Lens
import           Prelude (($), (==), fromIntegral, print)
import           System.IO (IO)
import           Web.Cookie

data Body = LazyByteString L.ByteString
type AuthHeader = ()

data WebmachineContext a = WebmachineContext
  { _contextSettings :: a
  }

data WebmachineState s = WebmachineState
  { _stateRequest :: Request
  , _stateStatusCode :: Status
  , _stateResponseHeaders :: ResponseHeaders
  , _stateUserState :: s
  }

type Webmachine c s = RWST (WebmachineContext c) () (WebmachineState s) IO

data Haltable a = ErrorResponse
                | StatusResponse
                | Continue a

data Resource c s auth body responseBody = Resource
  { _rServiceAvailable :: Webmachine c s (Haltable Bool)
  , _rUriTooLong :: Webmachine c s (Haltable Bool)
  , _rSupportedInputContentTypes :: H.HashMap ByteString (Request -> Webmachine c s (Maybe body))
  , _rSupportedOutputContentTypes :: H.HashMap ByteString (responseBody -> Body)
  , _rAuthorization :: Webmachine c s (Haltable (Either AuthHeader Bool))
  , _rHandler :: Maybe body -> Webmachine c s responseBody
  }

makeFields ''WebmachineContext
makeFields ''WebmachineState
makeFields ''Resource

decodeInput :: Resource c s a body r -> Webmachine c s (Maybe body)
decodeInput res = do
  req <- use request
  let mDecoder = contentTypeHeader >>= preview (folding $ splitWith (== fromIntegral (ord ';'))) >>= supportedTypes
      supportedTypes x = H.lookup x (res ^. supportedInputContentTypes)
      contentTypeHeader = L.lookup hContentType $ req ^. requestHeaders
  case mDecoder of
    Nothing -> return Nothing
    Just h -> h req

encodeOutput :: Resource c s a body r -> r -> Webmachine c s Body
encodeOutput res resp = do
  req <- use request
  let mEncoder = acceptHeader >>= \h -> supportedTypes h >>= \e -> return (h, e)
      supportedTypes x = H.lookup x $ res ^. supportedOutputContentTypes
      acceptHeader = L.lookup hAccept $ req ^. requestHeaders
  case mEncoder of
    Nothing -> do
      let anyEncoder = itoList (res ^. supportedOutputContentTypes) ^? traverse
      case anyEncoder of
        Nothing -> return $ LazyByteString "no supported accept type"
        Just (h, e) -> do
          Network.Webmachine.responseHeaders %= ((hContentType, h) :)
          return $ e resp
    Just (h, e) -> do
      Network.Webmachine.responseHeaders %= ((hContentType, h) :)
      return $ e resp

basic :: (Maybe body -> Webmachine c s responseBody) -> Resource c s auth body responseBody
basic handler = Resource
  { _rServiceAvailable = return $ Continue True
  , _rUriTooLong = return $ Continue False
  , _rSupportedInputContentTypes = mempty
  , _rSupportedOutputContentTypes = mempty
  , _rAuthorization = return $ Continue $ Right True
  , _rHandler = handler
  }

supportJSON :: (FromJSON body, ToJSON responseBody) => Resource c s auth body responseBody -> Resource c s auth body responseBody
supportJSON r = r
  & supportedInputContentTypes . at "application/json" ?~ (perform $ lazyRequestBody . to decode)
  & supportedOutputContentTypes . at "application/json" ?~ (LazyByteString . encode)

supportJSONInput :: (FromJSON body) => Resource c s auth body responseBody -> Resource c s auth body responseBody
supportJSONInput r = r
  & supportedInputContentTypes . at "application/json" ?~ (perform $ lazyRequestBody . to decode)

supportJSONOutput :: (ToJSON responseBody) => Resource c s auth body responseBody -> Resource c s auth body responseBody
supportJSONOutput r = r
  & supportedOutputContentTypes . at "application/json" ?~ (LazyByteString . encode)

runResource :: Resource c s auth body responseBody -> Webmachine c s Body
runResource r = do
  req <- use request
  let noContentType = (L.lookup hContentType $ req ^.requestHeaders) == Nothing
  mResult <- if (req ^. method) == methodGet || noContentType
    then fmap Just (r ^. handler $ Nothing)
    else do
      mIn <- r ^! act decodeInput
      case mIn of
        Nothing -> return Nothing
        Just _in -> fmap Just (r ^. handler $ mIn)
  case mResult of
    Nothing -> do
      Network.Webmachine.statusCode .= badRequest400
      "400 Bad Request" ^! to LazyByteString
    Just res -> res ^! act (encodeOutput r)

runWebmachine :: c -> s -> Request -> Webmachine c s a -> IO a
runWebmachine c s r m = fmap (^. _1) $ evalRWST m (WebmachineContext c) (WebmachineState r ok200 [] s)

makeResponse :: Body -> Webmachine c s Response
makeResponse (LazyByteString bs) = do
  code <- use Network.Webmachine.statusCode
  headers <- use Network.Webmachine.responseHeaders
  return $ responseLBS code headers bs

setCookie :: SetCookie -> Webmachine c s ()
setCookie s = Network.Webmachine.responseHeaders %= (("Set-Cookie", s ^. to renderSetCookie . to toByteString)  :)

getHeader :: HeaderName -> Webmachine c s [ByteString]
getHeader n = do
  hs <- use $ request . requestHeaders
  hs ^!! folded . filtered (\x -> x ^. _1 == n) . _2

