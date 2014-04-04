module Network.Wai.Lens (
  Wai.Request,
  Wai.Response,
  Wai.Application,
  Wai.Middleware,
  Wai.defaultRequest,
  Wai.responseLBS,
  Wai.responseSource,
  Wai.responseFile,
  Wai.responseBuilder,
  Wai.responseSourceBracket,
  _ChunkedBody,
  _KnownLength,
  method,
  httpVersion,
  requestHeaders,
  remoteHost,
  pathInfo,
  queryString,
  requestBody,
  requestBodyLength,
  requestHeaderHost,
  requestHeaderRange,
  lazyRequestBody,
  responseStatus,
  responseHeaders,
  responseToSource,
  filePartOffset,
  filePartByteCount,
  filePartFileSize
) where
import           Control.Category
import           Control.Lens
import           Control.Monad.Trans
import           Blaze.ByteString.Builder (Builder)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (Source, Flush)
import           Data.Maybe
import           Data.Text (Text)
import           Data.Word (Word64)
import           Network.HTTP.Types
import           Network.Socket (SockAddr)
import qualified Network.Wai as Wai
import           Prelude (Integer, ($))
import           System.IO (IO)

method :: Lens' Wai.Request Method
method = lens Wai.requestMethod $ \r s -> r { Wai.requestMethod = s }

httpVersion :: Lens' Wai.Request HttpVersion
httpVersion = lens Wai.httpVersion $ \r s -> r { Wai.httpVersion = s }

requestHeaders :: Lens' Wai.Request RequestHeaders
requestHeaders = lens Wai.requestHeaders $ \r s -> r { Wai.requestHeaders = s }

remoteHost :: Lens' Wai.Request SockAddr
remoteHost = lens Wai.remoteHost $ \r s -> r { Wai.remoteHost = s }

pathInfo :: Lens' Wai.Request [Text]
pathInfo = lens Wai.pathInfo $ \r s -> r { Wai.pathInfo = s }

queryString :: Lens' Wai.Request Query
queryString = lens Wai.queryString $ \r s -> r { Wai.queryString = s }

requestBody :: Lens' Wai.Request (Source IO ByteString)
requestBody = lens Wai.requestBody $ \r s -> r { Wai.requestBody = s }

{-
vault :: Lens' Wai.Request Vault
vault = lens Wai.vault $ \r s -> r { Wai.vault = s }
-}

requestBodyLength :: Lens' Wai.Request Wai.RequestBodyLength
requestBodyLength = lens Wai.requestBodyLength $ \r s -> r { Wai.requestBodyLength = s }

requestHeaderHost :: Lens' Wai.Request (Maybe ByteString)
requestHeaderHost = lens Wai.requestHeaderHost $ \r s -> r { Wai.requestHeaderHost = s }

requestHeaderRange :: Lens' Wai.Request (Maybe ByteString)
requestHeaderRange = lens Wai.requestHeaderRange $ \r s -> r { Wai.requestHeaderRange = s }

_ChunkedBody :: Prism' Wai.RequestBodyLength ()
_ChunkedBody = prism' (\_ -> Wai.ChunkedBody) $ \x -> case x of
  Wai.ChunkedBody -> Just ()
  _ -> Nothing

_KnownLength :: Prism' Wai.RequestBodyLength Word64
_KnownLength = prism' Wai.KnownLength $ \x -> case x of
  Wai.KnownLength y -> Just y
  _ -> Nothing

lazyRequestBody :: MonadIO m => Action m Wai.Request L.ByteString
lazyRequestBody = act (liftIO . Wai.lazyRequestBody)

responseStatus :: Getter Wai.Response Status
responseStatus = to Wai.responseStatus

responseHeaders :: Getter Wai.Response ResponseHeaders
responseHeaders = to Wai.responseHeaders

responseToSource :: Getter Wai.Response (Status, ResponseHeaders, Wai.WithSource IO (Flush Builder) b)
responseToSource = to Wai.responseToSource

filePartOffset :: Lens' Wai.FilePart Integer
filePartOffset = lens Wai.filePartOffset $ \r s -> r { Wai.filePartOffset = s }

filePartByteCount :: Lens' Wai.FilePart Integer
filePartByteCount = lens Wai.filePartByteCount $ \r s -> r { Wai.filePartByteCount = s }

filePartFileSize :: Lens' Wai.FilePart Integer
filePartFileSize = lens Wai.filePartFileSize $ \r s -> r { Wai.filePartFileSize = s }
