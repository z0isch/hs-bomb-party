{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module WithPlayerApi (PlayerId (..), API, withPlayerApi) where

import CustomPrelude

import Control.Monad.Error.Class (MonadError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Network.URI (parseURIReference)
import qualified Network.Wai as Wai
import qualified RIO.Text as T
import Servant
import Servant.HTML.Lucid
import Servant.Server.Internal.Delayed (passToServer)
import Text.Shakespeare.Text (ToText (..))
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies)

newtype PlayerId = PlayerId {getPlayerId :: UUID}
    deriving stock (Eq, Show)
    deriving newtype (Hashable, ToJSON, FromJSON)

instance ToText PlayerId where
    toText (PlayerId i) = toText $ UUID.toText i

instance FromHttpApiData PlayerId where
    parseQueryParam = maybe (Left "PlayerId not a valid UUID") (Right . PlayerId) . UUID.fromText

type API a =
    RequestURI
        :> Header "Cookie" Text
        :> QueryParam "referer" HttpApiDataURI
        :> (LoginAPI :<|> a)

type LoginAPI =
    "login"
        :> Verb
            GET
            302
            '[HTML]
            ( Headers
                '[ Header "Set-Cookie" SetCookie
                 , Header "Location" HttpApiDataURI
                 ]
                NoContent
            )

withPlayerApi ::
    forall a m.
    ( MonadIO m
    , PlayerIdRedirect (ServerT a m)
    , PlayerIdRedirect (ServerT LoginAPI m)
    ) =>
    Proxy a ->
    (PlayerId -> ServerT a m) ->
    ServerT (API a) m
withPlayerApi _ a reqURI mCookies mReferer = login :<|> aServer
  where
    referer = Data.Maybe.fromMaybe (HttpApiDataURI $ URI mempty Nothing "/" "" "") mReferer
    addRootSlash u = u{uriPath = "/" <> uriPath u}
    loginURI =
        safeLink'
            (addRootSlash . linkURI)
            (Proxy @(QueryParam "referer" HttpApiDataURI :> (LoginAPI :<|> a)))
            (Proxy @(QueryParam "referer" HttpApiDataURI :> LoginAPI))
            (Just $ HttpApiDataURI reqURI)
    aServer :: ServerT a m
    aServer = case mPlayerId of
        Nothing ->
            redirect loginURI
        Just playerId -> a playerId
    playerCookieName = "X-PLAYER-ID"
    mPlayerId =
        fmap PlayerId
            . UUID.fromASCIIBytes
            =<< lookup playerCookieName
            . parseCookies
            . encodeUtf8
            =<< mCookies
    login :: ServerT LoginAPI m
    login = case mPlayerId of
        Nothing -> do
            newPlayerId <- liftIO nextRandom
            pure
                $ addHeader
                    defaultSetCookie
                        { setCookieName = playerCookieName
                        , setCookieValue = encodeUtf8 $ UUID.toText newPlayerId
                        }
                $ addHeader referer NoContent
        Just _ -> redirect $ getHttpApiDataURI referer

-- | Similar to `ThrowAll` from `servant-auth`- allows redirecting across a sub-site
class PlayerIdRedirect a where
    redirect :: URI -> a

instance (PlayerIdRedirect a, PlayerIdRedirect b) => PlayerIdRedirect (a :<|> b) where
    redirect l = redirect l :<|> redirect l

instance {-# OVERLAPPING #-} (PlayerIdRedirect b) => PlayerIdRedirect (a -> b) where
    redirect l _ = redirect l

instance {-# OVERLAPPABLE #-} (MonadError ServerError m) => PlayerIdRedirect (m a) where
    redirect l =
        throwError
            $ err302
                { errHeaders =
                    ("Location", encodeUtf8 $ T.pack $ show l) : errHeaders err302
                }

instance PlayerIdRedirect (RIO app m) where
    redirect l =
        throwM
            $ err302
                { errHeaders =
                    ("Location", encodeUtf8 $ T.pack $ show l) : errHeaders err302
                }

data RequestURI

instance (HasServer api ctx) => HasServer (RequestURI :> api) ctx where
    type ServerT (RequestURI :> api) m = URI -> ServerT api m

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s

    route _ ctx d = route (Proxy @api) ctx $ passToServer d requestURI

requestURI :: Wai.Request -> URI
requestURI r =
    URI
        mempty
        Nothing
        (T.unpack $ decodeUtf8 $ Wai.rawPathInfo r)
        (T.unpack $ decodeUtf8 $ Wai.rawQueryString r)
        ""

newtype HttpApiDataURI = HttpApiDataURI {getHttpApiDataURI :: URI}

instance FromHttpApiData HttpApiDataURI where
    parseQueryParam = coerce @(Text -> Either Text URI) $ maybe (Left "invalid uri") Right . parseURIReference . T.unpack

instance ToHttpApiData HttpApiDataURI where
    toQueryParam = coerce @(URI -> Text) $ T.pack . show
