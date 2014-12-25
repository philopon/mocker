{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}

import Control.Arrow
import Control.Applicative
import Control.Monad.Apiary.Action
import Web.Apiary
import Web.Apiary.Heroku
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy  as L
import qualified Data.CaseInsensitive as CI
import Text.Hastache
import Data.FileEmbed

defaultTemplate :: T.Text
defaultTemplate = T.decodeUtf8 $(embedFile "defaultTemplate.txt")

parseHeader :: S.ByteString -> Maybe (CI.CI S.ByteString, S.ByteString)
parseHeader h = case S.span (/= ':') h of
    (_, "")       -> Nothing
    (name, value) -> Just (CI.mk name, S.tail value)

addExtraHeader :: Monad m => CI.CI S.ByteString -> S.ByteString -> ActionT exts prms m ()
addExtraHeader "content-Type" ct = contentType ct
addExtraHeader name value        = addHeader name value

muListTuple :: (MuVar k, MuVar v, Monad m) => (T.Text -> Bool) -> (T.Text -> Bool) -> [(k,v)] -> MuType m
muListTuple isKey isVal = MuList . map sub
  where
    sub (k, v) n | isKey n = return $ MuVariable k
                 | isVal n = return $ MuVariable v
                 | otherwise = return MuNothing

main :: IO ()
main = runHeroku run def $ do
    [capture|/mock/**|]
    . ([key|status|]         ?? "set status code of response" =?!: statusCode status200)
    . ([key|status-message|] ?? "set status message of response" =?!: statusMessage status200)
    . ([key|content-type|]   ?? "set content-type of response" =?!: ("text/plain" :: S.ByteString))
    . ([key|header|]         ?? "additional headers of response" =*: pByteString)
    . ([key|template|]       ?? "template of response body" =?!: defaultTemplate)
    . action $ do
        (status', statusMessage') <- [params|status,status-message|]
        status $ Status status' statusMessage'

        contentType' <- param [key|content-type|]
        contentType contentType'

        headers <- mapMaybe parseHeader <$> param [key|header|]
        mapM_ (uncurry addExtraHeader) headers

        let dict = flip S.member . S.fromList
            isStatusMessage   = dict [ "statusMessage", "status-message"
                                     , "statusMsg", "status-msg"
                                     ]
            isContentType     = dict [ "contentType", "content-type"
                                     ]
            isResponseHeaders = dict [ "responseHeaders", "response-Headers"
                                     , "respHeaders", "resp-headers"
                                     , "rspHeaders", "rsp-Headers"
                                     , "rspHdrs", "rsp-hdrs"
                                     ]
            isRequestHeaders  = dict [ "requestHeaders", "request-Headers"
                                     , "reqHeaders", "req-headers"
                                     , "rqHeaders", "rq-headers"
                                     , "rqhdrs", "rq-hdrs"
                                     ]
            isQueryParams     = dict [ "queryParameters", "query-parameters"
                                     , "queryParams", "query-Params"
                                     ]
            isReqBodyType     = dict [ "request-body-type", "requestBodyType"
                                     , "req-body-type", "reqBodyType"
                                     ]
            isRequestMethod   = dict [ "request-method", "method", "meth" ]
            isHttpVersion     = dict [ "http-version", "version", "ver" ]
            isRemoteHost      = dict [ "remoteHost", "remote-host", "host" ]

            headerList        = muListTuple (flip elem ["key", "name"]) (flip elem ["value", "val"])

            cxt name = case CI.mk name of
                n | n == "status"       -> return $ MuVariable status'
                  | isStatusMessage   n -> return $ MuVariable statusMessage'
                  | isContentType     n -> return $ MuVariable contentType'

                  | isResponseHeaders n -> return . headerList $ map (first CI.original) headers
                  | isRequestHeaders  n -> headerList . map (first CI.original) <$> getHeaders
                  | isQueryParams     n -> headerList <$> getQueryParams
                  | isReqBodyType     n -> getReqBody >>= return . \case
                    Unknown       _ -> MuVariable ("unknown" :: S.ByteString)
                    UrlEncoded  _ _ -> MuVariable ("url-encoded" :: S.ByteString)
                    Multipart _ _ _ -> MuVariable ("multipart" :: S.ByteString)
                  | isRequestMethod   n -> MuVariable . Wai.requestMethod <$> getRequest
                  | isHttpVersion     n -> MuVariable . show . Wai.httpVersion <$> getRequest
                  | isRemoteHost      n -> MuVariable . show . Wai.remoteHost  <$> getRequest

                  | otherwise           -> return MuNothing

        files <- getReqBodyFiles
        template <- case filter (("template" ==) . fileParameter) files of
            []  -> param [key|template|]
            b:_ -> return (T.decodeUtf8 . L.toStrict $ fileContent b)

        hastacheStr defaultConfig template cxt >>= lazyText
