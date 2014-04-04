{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Network.Webmachine.DecisionCore where
import           Control.Exception
import           Control.Lens hiding (Context)
import           Control.Monad.Cont
import           Control.Monad.RWS.Strict
import           Crypto.Conduit (sinkHash)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Lazy (fromStrict)
import           Data.Conduit (($$), runResourceT)
import           Data.Digest.Pure.MD5 (MD5Digest)
import qualified Data.HashMap.Strict as H
import           Data.Text (Text)
import           Data.Time.Clock
import qualified Data.Serialize as S
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai.Lens

type NonEmptyList h a = (a, [a])

data Result e h a
  = Error SomeException
  | Halt Status h
  | Result a

class MonadIO m => DecisionGraph m r where
  ping :: Context r m Bool
  ping = return True

  serviceAvailable :: Context r m Bool
  serviceAvailable = return True

  knownMethods :: Context r m [Method]
  knownMethods = return [ methodGet, methodHead, methodPost, methodPut, methodDelete, methodPatch, methodTrace, methodConnect, methodOptions ]

  uriTooLong :: Context r m Bool
  uriTooLong = return False

  allowedMethods :: Context r m [Method]
  allowedMethods = return [ methodGet, methodHead ]

  validateContentChecksum :: Context r m (Maybe Bool)
  validateContentChecksum = return Nothing

  malformedRequest :: Context r m Bool
  malformedRequest = return False

  isAuthorized :: Context r m (Result e h (Either ByteString ()))
  isAuthorized = return True

  forbidden :: Context r m Bool
  forbidden = return False

  validContentHeaders :: Context r m Bool
  validContentHeaders = return True

  knownContentType :: Context r m Bool
  knownContentType = return True

  validEntityLength :: Context r m Bool
  validEntityLength = return True

  options :: Context r m [Method]
  options = return []

  languageAvailable :: Context r m Bool
  languageAvailable = return True

  resourceExists :: Context r m Bool
  resourceExists = return True

  generateETag :: Context r m (Maybe ByteString)
  generateETag = return Nothing

  lastModified :: Context r m (Maybe UTCTime)
  lastModified = return Nothing

  movedPermanently :: Context r m Bool
  movedPermanently = return False

  previouslyExisted :: Context r m Bool
  previouslyExisted = return False

  movedTemporarily :: Context r m Bool
  movedTemporarily = return False

  allowMissingPost :: Context r m Bool
  allowMissingPost = return False

  deleteResource :: Context r m Bool
  deleteResource = return False

  deleteCompleted :: Context r m Bool
  deleteCompleted = return True

  postIsCreate :: Context r m Bool
  postIsCreate = return False

  createPath :: Context r m (Maybe [Text])
  createPath = return Nothing

  baseUri :: Context r m (Maybe [Text])
  baseUri = return Nothing

  processPost :: Context r m Bool
  processPost = return False

  isConflict :: Context r m Bool
  isConflict = return False

  expires :: Context r m (Maybe UTCTime)
  expires = return Nothing

  multipleChoices :: Context r m Bool
  multipleChoices = return False

  contentTypesAccepted :: Context r m [ByteString]
  contentTypesAccepted = return []

  charsetsProvided :: Context r m (H.HashMap ByteString (ByteString -> ByteString))
  charsetsProvided = return []

  encodingsProvided :: Context r m (H.HashMap ByteString (ByteString -> ByteString))
  encodingsProvided = return [("identity", id)]

  contentTypesProvided :: Context r m (H.HashMap ByteString (Result e h a -> ByteString))
  contentTypesProvided = return []

  variances :: Context r m [()]
  variances = return []

type ContextState = (Request, H.HashMap HeaderName ByteString)

request :: Lens' ContextState Request
request = _1

resultHeaders :: Lens' ContextState (H.HashMap HeaderName ByteString)
resultHeaders = _2

newtype Context r m a = Context
  { fromContext :: ContT Response (RWST r () ContextState m) a
  }
  deriving (Monad)

instance (Monad m) => MonadState ContextState (Context r m) where
  get = Context $ lift get
  put = Context . lift . put

instance MonadIO m => MonadIO (Context r m) where
  liftIO = Context . liftIO

instance DecisionGraph IO ()

{-
runDecisionGraph :: DecisionGraph m r => r -> m Response
runDecisionGraph = runReaderT (runContT (decisionGraph True) return)

decisionGraph :: DecisionGraph m r => Context r m Response
decisionGraph = callCC $ \exit -> do
  b13 exit
  return $ 
-}

class Decision m a | a -> m where
  decision :: a -> m Response

instance Monad m => Decision (Context r m) Status where
  decision s = return $ responseLBS s [] $ fromStrict $ statusMessage s

instance Monad m => Decision (Context r m) (Context r m Response) where
  decision s = s

findHeader :: HeaderName -> [Header] -> Maybe BS.ByteString
findHeader h hs = headerPair ^? _Just . _2
  where headerPair = findOf each (\(k, v) -> k == h) hs

decisionTest :: (Monad m, Decision m a, Decision m b) => m Bool -> a -> b -> m Response
decisionTest t = decisionTest' t id

decisionTest' :: (Monad m, Decision m a, Decision m b) => m t -> (t -> Bool) -> a -> b -> m Response
decisionTest' t tf trueFlow falseFlow = do
  tr <- t
  if tf tr then decision trueFlow else decision falseFlow

getMethod :: Monad m => Context r m Method
getMethod = use $ request . method

getHeader :: Monad m => HeaderName -> Context r m (Maybe BS.ByteString)
getHeader h = use $ request . requestHeaders . to (findHeader h)

setHeader :: Monad m => HeaderName -> ByteString -> Context r m ()

chooseCharset
cooseEncoding :: 
variances

-- | Service available
b13 :: (DecisionGraph m r) => Context r m Response
b13 = decisionTest ping b13b serviceUnavailable503

-- | Service available, part 2
b13b :: (DecisionGraph m r) => Context r m Response
b13b = decisionTest serviceAvailable b12 serviceUnavailable503

-- | Known method?
b12 :: (DecisionGraph m r) => Context r m Response
b12 = do
  m <- getMethod
  decisionTest (fmap (elem m) knownMethods) b11 notImplemented501
  where
    knownMethod = undefined

-- | URI too long?
b11 :: (DecisionGraph m r) => Context r m Response
b11 = decisionTest uriTooLong requestURITooLong414 b10

-- | Method allowed?
b10 :: (DecisionGraph m r) => Context r m Response
b10 = do
  methods <- allowedMethods
  m <- use $ request . method
  if elem m methods
    then b9
    else do
      (resultHeaders . at "Allow") .= Just (BS.intercalate ", " methods)
      decision methodNotAllowed405

-- | Content-MD5 present?
b9 :: (DecisionGraph m r) => Context r m Response
b9 = do
  h <- uses (request . requestHeaders) $ findHeader hContentMD5
  maybe b9b b9a h

-- | Content-MD5 valid?
b9a :: (MonadIO m, DecisionGraph m r) => ByteString -> Context r m Response
b9a providedMD5 = do
  bodySource <- use $ request . requestBody
  digest <- liftIO $ bodySource $$ sinkHash
  if fst (B16.decode providedMD5) == S.encode (digest :: MD5Digest)
    then b9b
    else decision badRequest400

-- | Malformed?
b9b :: (DecisionGraph m r) => Context r m Response
b9b = decisionTest malformedRequest badRequest400 b8

-- | Authorized?
b8 :: (DecisionGraph m r) => Context r m Response
b8 = do
  authResult <- isAuthorized
  case authResult of
    Result r -> case r of
      Left authHead -> do
        setHeader "WWW-Authenticate" authHead
        decision unauthorized401
      Right u -> b7
    _ -> return authResult

-- | Forbidden?
b7 :: (DecisionGraph m r) => Context r m Response
b7 = decisionTest forbidden forbidden403 b6

-- | Okay Content-* headers?
b6 :: (DecisionGraph m r) => Context r m Response
b6 = decisionTest validContentHeaders b5 notImplemented501

-- | Known Content-Type?
b5 :: (DecisionGraph m r) => Context r m Response
b5 = decisionTest knownContentType b4 unsupportedMediaType415

-- | Request entity too large?
b4 :: (DecisionGraph m r) => Context r m Response
b4 = decisionTest validEntityLength b3 requestEntityTooLarge413

-- | OPTIONS?
b3 :: (DecisionGraph m r) => Context r m Response
b3 = do
  m <- use $ request . method
  if m == methodOptions
    then decision internalServerError500
    else c3

-- | Accept exists
c3 :: (DecisionGraph m r) => Context r m Response
c3 = do
  types <- contentTypesProvided
  mAccept <- uses (request . requestHeaders) $ findHeader "Accept"
  case mAccept of
    Just a -> c4 a
    Nothing -> case types ^? traverse of
      Nothing -> decision notAcceptable406
      Just (t, _) -> do
        (resultHeaders . at hContentType) .= Just t
        d4

c4 :: (DecisionGraph m r) => ByteString -> Context r m Response
c4 contentType = do
  types <- contentTypesProvided
  case _ of
    Nothing -> decision notAcceptable406
    Just m -> do
      setMetadata "Content-Type" m
      d4

d4 :: (DecisionGraph m r) => Context r m Response
d4 = decisionTest' (getHeader "Accept-Language") (== Nothing) e5 d5

d5 :: (DecisionGraph m r) => Context r m Response
d5 = decisionTest languageAvailable e5 notAcceptable406

e5 :: (DecisionGraph m r) => Context r m Response
e5 = do
  mh <- getHeader "Accept-Charset"
  case mh of
    Nothing -> decisionTest' (chooseCharset "*") (== Nothing) notAcceptable406 f6
    Just h -> e6

e6 :: (DecisionGraph m r) => Context r m Response
e6 = decisionTest' (getHeader "Accept-Charset" >>= chooseCharset) (== Nothing ) notAcceptable406 f6

-- TODO implement this correctly
f6 :: (DecisionGraph m r) => ByteString -> Context r m Response
f6 h = g7

f7 :: (DecisionGraph m r) => Context r m Response
f7 = decisionTest' (getHeader "Accept-Encoding" >>= chooseEncoding) (== Nothing) notAcceptable406 g7

g7 :: (DecisionGraph m r) => ByteString -> Context r m Response
g7 = do
  vs <- variances
  case vs of
    [] -> return ()
    _ -> setHeader "Vary" (BS.intercalate vs ", ")
  decisionTest resourceExists g8 h7

g8 :: (DecisionGraph m r) => ByteString -> Context r m Response
g8 = decisionTest' (getHeader "If-Match") (== Nothing) h10 g9

g9 :: (DecisionGraph m r) => ByteString -> Context r m Response
g9 = decisionTest' (getHeader "If-Match") (== (Just "*")) h10 (g11 h)

g11 :: (DecisionGraph m r) => ByteString -> Context r m Response
g11 h = do
  let etags = splitQuotedStrings h
  etag <- generateETag
  if elem etag etags
    then h10
    else decision preconditionFailed412

h7 :: (DecisionGraph m r) => ByteString -> Context r m Response
h7 = decisionTest' (getHeader "If-Match") (== Nothing) i7 preconditionFailed412

h10 :: (DecisionGraph m r) => ByteString -> Context r m Response
h10 = decisionTest' (getHeader "If-Unmodified-Since") (== Nothing) i12 (h11 h)

h11 :: (DecisionGraph m r) => ByteString -> Context r m Response
h11 h = case convertRequestDate h of
  Nothing -> i12
  Just t -> h12 t

h12 :: (DecisionGraph m r) => UTCTime -> Context r m Response
h12 t = do
  lm <- lastModified
  case lm of
    Nothing -> i12
    Just l -> if t > t
      then decision preconditionFailed412
      else i12

i4 :: (DecisionGraph m r) => Context r m Response
i4 = do
  res <- movedPermanently
  case res of
    Result r -> case r of
      Just uri -> do
        setHeader "Location" uri
        decision movedPermanently301
      Nothing -> p3
    _ -> return res

i7 :: (DecisionGraph m r) => Context r m Response
i7 = decisionTest' getMethod (== methodPut) i4 k7

i12 :: (DecisionGraph m r) => Context r m Response
i12 = decisionTest' (getHeader "If-None-Match") (== Nothing) l13 i13

i13 :: (DecisionGraph m r) => Context r m Response
i13 = decisionTest' (getHeader "If-None-Match") (== (Just "*")) j18 k13

j18 :: (DecisionGraph m r) => Context r m Response
j18 = decisionTest' getMethod (\m -> m == methodGet || m == methodHead) notModified304 preconditionFailed412

k5 :: (DecisionGraph m r) => Context r m Response
k5 = do
  res <- movedPermanently
  case res of
    Result r -> case r of
      Just uri -> do
        setHeader "Location" uri
        decision movedPermanently301
      Nothing -> l5
    _ -> return res

k7 :: (DecisionGraph m r) => Context r m Response
k7 = decisionTest previouslyExisted k5 l7

-- TODO
k13 :: (DecisionGraph m r) => Context r m Response
k13 = l13

l5 :: (DecisionGraph m r) => Context r m Response
l5 = do
  res <- movedTemporarily
  case res of
    Result r -> case r of
      Just uri -> do
        setHeader "Location" uri
        decision temporaryRedirect307
      Nothing -> m5
    _ -> return res

l7 :: (DecisionGraph m r) => Context r m Response
l7 = decisionTest' getMethod (== methodPost) m7 notFound404

l13 :: (DecisionGraph m r) => Context r m Response
l13 = decisionTest' (getHeader "If-Modified-Since") (== Nothing) m16 l14

l14 :: (DecisionGraph m r) => Context r m Response
l14 dateHeader = do
  case convertRequestDate dateHeader of
    Nothing -> m16
    Just d -> l15 d

l15 :: (DecisionGraph m r) => Context r m Response
l15 t = do
  now <- liftIO getCurrentTime
  if t > now
    then m16
    else l17 t

l17 :: (DecisionGraph m r) => Context r m Response
l17 t = do
  lm <- lastModified
  case lm of
    Nothing -> decision notModified304
    Just m -> if m > t then m16 else decision notModified304

m5 :: (DecisionGraph m r) => Context r m Response
m5 = decisionTest getMethod (== methodPost) n5 gone410

m7 :: (DecisionGraph m r) => Context r m Response
m7 = decisionTest allowMissingPost n11 notFound404

m16 :: (DecisionGraph m r) => Context r m Response
m16 = decisionTest getMethod (== methodDelete) m20 n16

m20 :: (DecisionGraph m r) => Context r m Response
m20 = decisionTest deleteResource m20b internalServerError500

m20b :: (DecisionGraph m r) => Context r m Response
m20b = decisionTest deleteCompleted o20 accepted202

n5 :: (DecisionGraph m r) => Context r m Response
n5 = decisionTest allowMissingPost n11 gone410

-- TODO
n11 :: (DecisionGraph m r) => Context r m (Result ByteString h a)
n11 = do
  {-
  shouldCreate <- postIsCreate
  if shouldCreate
    then do
      path <- createPath
      case path of
        Nothing -> return $ Error "post is create without create path"
        Just p -> do
          base <- baseUri
          let b = if BS.last base == '/' then BS.init base else base
  -}
  p11


n16 :: (DecisionGraph m r) => Context r m Response
n16 = decisionTest' getMethod (== methodPost) n11 o16

o14 :: (DecisionGraph m r) => Context r m Response
o14 = do
  conflict <- isConflict
  if conflict
    then decision conflict409
    else do
      res <- acceptHelper
      case res of
        Result a -> p11 a
        _ -> return res

o16 :: (DecisionGraph m r) => Context r m Response
o16 = decisionTest' getMethod (== methodPut) o14 o18

o18 :: (DecisionGraph m r) => Context r m Response
o18 = do
  o18b

o18b :: (DecisionGraph m r) => Context r m Response
o18b = decisionTest multipleChoices multipleChoices300 ok200

o20 :: (DecisionGraph m r) => Context r m Response
o20 = decisionTest hasResponseBody o18 noContent204

p3 :: (DecisionGraph m r) => Context r m Response
p3 = do
  conflict <- isConflict
  if conflict
    then decision conflict409
    else do
      res <- acceptHelper
      case res of
        Result a -> p11 a
        _ -> return res

p11 :: (DecisionGraph m r) => Context r m Response
p11 = decisionTest' (getRequestHeader "Location") (== Nothing) o20 created201

