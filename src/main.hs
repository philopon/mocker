{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Control.Applicative
import Control.Monad.Apiary.Action
import Web.Apiary
import Web.Apiary.Heroku
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy  as L
import qualified Data.CaseInsensitive as CI
import Text.Hastache
import Data.FileEmbed
import qualified Data.Map.Strict as M

defaultTemplate :: T.Text
defaultTemplate = T.decodeUtf8 $(embedFile "defaultTemplate.txt")

parseHeader :: S.ByteString -> Maybe (S.ByteString, S.ByteString)
parseHeader h = case S.span (/= ':') h of
    (_, "")       -> Nothing
    (name, value) -> Just (name, S.tail value)

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
    . ([key|status|]        ?? "set status code of response" =?!: statusCode status200)
    . ([key|statusMessage|] ?? "set status message of response" =?!: statusMessage status200)
    . ([key|contentType|]   ?? "set content-type of response" =?!: ("text/plain" :: S.ByteString))
    . ([key|header|]        ?? "additional headers of response" =*: pByteString)
    . ([key|template|]      ?? "template of response body" =?!: defaultTemplate)
    . action $ do
        (status', statusMessage') <- [params|status,statusMessage|]
        status $ Status status' statusMessage'

        contentType' <- param [key|contentType|]
        contentType contentType'

        respHeaders <- mapMaybe parseHeader <$> param [key|header|]
        mapM_ (uncurry addExtraHeader . first CI.mk) respHeaders

        reqHeaders <- getHeaders
        qParams    <- getQueryParams
        reqBody    <- getReqBody
        rawRequest <- getRequest

        let kStatusMessage   = [ "statusMessage", "status-message"
                               , "statusMsg", "status-msg"
                               ]
            kContentType     = [ "contentType", "content-type"
                               ]
            kResponseHeader  = [ "responseHeader", "response-Header"
                               , "respHeader", "resp-header"
                               , "rspHeader", "rsp-Header"
                               , "rspHdr", "rsp-hdr"
                               ]
            kResponseHeaders = map (CI.mk . flip T.snoc 's') kResponseHeader
            kRequestHeader   = [ "requestHeader", "request-Header"
                               , "reqHeader", "req-header"
                               , "rqHeader", "rq-header"
                               , "rqhdr", "rq-hdr"
                               ]
            kRequestHeaders  = map (CI.mk . flip T.snoc 's') kRequestHeader
            kQueryParam       = [ "queryParameter", "query-parameter"
                                , "queryParam", "query-Param"
                                ]
            kQueryParams      = map (CI.mk . flip T.snoc 's') kQueryParam
            kReqBodyType      = [ "request-body-type", "requestBodyType"
                                , "req-body-type", "reqBodyType"
                                ]
            kRequestMethod    = [ "request-method", "method", "meth" ]
            kHttpVersion      = [ "http-version", "version", "ver" ]
            kRemoteHost       = [ "remoteHost", "remote-host", "host" ]

            headerList        = muListTuple (flip elem ["key", "name"]) (flip elem ["value", "val"])

            dictionary = M.unions
                [ M.singleton "status" (MuVariable status')
                , M.fromList $ map (,MuVariable status') kStatusMessage
                , M.fromList $ map (,MuVariable contentType') kContentType

                , M.fromList $ map (,headerList $ respHeaders) kResponseHeaders
                , M.fromList [ (CI.mk $ T.concat [k, ".", T.decodeUtf8 n], MuVariable v)
                             | k     <- kResponseHeader
                             , (n,v) <- respHeaders
                             ]

                , M.fromList $ map (,headerList $ map (first CI.original) reqHeaders) kRequestHeaders
                , M.fromList [ (CI.mk $ T.concat [k, ".", T.decodeUtf8 $ CI.original n], MuVariable v)
                             | k     <- kRequestHeader
                             , (n,v) <- reqHeaders
                             ]

                , M.fromList $ map (,headerList $ qParams) kQueryParams
                , M.fromList [ (CI.mk $ T.concat [k, ".", T.decodeUtf8 n], MuVariable v)
                             | k     <- kQueryParam
                             , (n,v) <- qParams
                             ]

                , M.fromList $ map (, case reqBody of
                    Unknown       _ -> MuVariable ("unknown" :: S.ByteString)
                    UrlEncoded  _ _ -> MuVariable ("url-encoded" :: S.ByteString)
                    Multipart _ _ _ -> MuVariable ("multipart" :: S.ByteString)
                    ) kReqBodyType
                , M.fromList $ map (,MuVariable $ Wai.requestMethod      rawRequest) kRequestMethod
                , M.fromList $ map (,MuVariable . show $ Wai.httpVersion rawRequest) kHttpVersion
                , M.fromList $ map (,MuVariable . show $ Wai.remoteHost  rawRequest) kRemoteHost
                ]

            cxt n = return . maybe MuNothing id $ M.lookup (CI.mk n) dictionary

        files <- getReqBodyFiles
        template <- case filter (("template" ==) . fileParameter) files of
            []  -> param [key|template|]
            b:_ -> return (T.decodeUtf8 . L.toStrict $ fileContent b)

        liftIO (hastacheStr defaultConfig template cxt) >>= lazyText
