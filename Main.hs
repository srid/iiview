{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Clay ((?), Css, em, pc, pct, px, sym)
import qualified Clay as C
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Time
import Data.Time.Clock.POSIX
import Development.Shake
import Lucid
import Path
import Rib (MMark, Source)
import qualified Rib
import qualified Rib.Parser.MMark as MMark
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (div, some)

type Parser = Parsec Void Text

data Channel
  = Channel
      { _channel_name :: Text,
        _channel_logs :: Map Day [Log]
      }
  deriving (Eq, Show)

-- | Parse the channel name out of its "out" filename (eg: "#nixos/")
channelDirNameParser :: Parser Text
channelDirNameParser = do
  _ <- some (char '#')
  toText <$> someTill printChar (char '/')

data Log
  = Log
      { _log_time :: UTCTime,
        _log_msg :: Text
      }
  deriving (Eq, Show)

logParser :: Parser Log
logParser = do
  Just ts' :: Maybe Int <- fmap readMaybe $ some digitChar
  let ts = posixSecondsToUTCTime $ fromIntegral ts'
  msg <- toText <$> someTill anySingle eof
  pure $ Log ts msg

parseChannel :: (MonadIO m, MonadFail m) => Path b File -> m (Either Text Channel)
parseChannel fp = do
  let chDirName = toText $ toFilePath $ dirname $ parent fp
  case parse channelDirNameParser "" chDirName of
    Left e ->
      pure $ Left $ toText $ errorBundlePretty e
    Right chName -> do
      logsRaw <- lines <$> readFileText (toFilePath fp)
      logs <- flip traverse logsRaw $ \logRaw -> do
        case parse logParser "" logRaw of
          Left e ->
            fail $ errorBundlePretty e
          Right log ->
            pure log
      pure $ Right $ Channel chName
        $ fmap (sortOn _log_time)
        $ Map.fromListWith (<>)
        $ (\l -> (utctDay $ _log_time l, [l])) <$> logs

-- | This will be our type representing generated pages.
--
-- Each `Source` specifies the parser type to use. Rib provides `MMark` and
-- `Pandoc`; but you may define your own as well.
data Page
  = Page_Index [Source Channel]
  | Page_Channel (Source Channel)
  | Page_ChannelDay Channel Day [Log]

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `a`, from which static files will be read.
-- 2. Directory `b`, under which target files will be generated.
-- 3. Shake action to run.
--
-- In the shake build action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = Rib.run [reldir|a|] [reldir|b|] generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles [[relfile|static/**|]]
  -- Build individual sources, generating .html for each.
  -- The function `buildHtmlMulti` takes the following arguments:
  -- - Function that will parse the file (here we use mmark)
  -- - File patterns to build
  -- - Function that will generate the HTML (see below)
  chs <-
    Rib.forEvery [[relfile|#*/out|]] $ \fp -> do
      chSrc <- Rib.buildHtml' fp parseChannel channelOutFile $ renderPage . Page_Channel
      let ch = Rib.sourceVal chSrc
      forM_ (Map.toList $ _channel_logs ch) $ \(day, logs) -> do
        dayFp <- liftIO $ parseRelFile $ toString $ _channel_name ch <> "/" <> show day <> ".html"
        Rib.writeHtml dayFp
          $ renderPage
          $ Page_ChannelDay ch day logs
      pure chSrc
  -- Write an index.html linking to the aforementioned files.
  Rib.writeHtml [relfile|index.html|]
    $ renderPage
    $ Page_Index chs
  where
    channelOutFile =
      liftIO . parseRelFile . toString . (<> ".html") . _channel_name

-- | Define your site HTML here
renderPage :: Page -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css"
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
    title_ $ case page of
      Page_Index _ -> "ii viewer"
      Page_Channel (Rib.sourceVal -> ch) -> do
        toHtml $ _channel_name ch
        " - ii viewer"
      Page_ChannelDay ch day _ -> do
        toHtml $ show @Text day
        " - "
        toHtml $ _channel_name ch
        " - ii viewer"
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    with div_ [class_ "ui text container", id_ "thesite"] $ do
      case page of
        Page_Index chs -> div_ $ forM_ chs $ \ch ->
          with li_ [class_ "pages"] $ do
            b_ $ with a_ [href_ $ Rib.sourceUrl ch] $ do
              toHtml $ _channel_name $ Rib.sourceVal ch
        Page_Channel (Rib.sourceVal -> ch) -> do
          h1_ $ toHtml $ _channel_name ch
          forM_ (Map.keys $ _channel_logs ch) $ \day -> do
            li_ $ do
              -- TODO: refactor `Source` so we can use it for source-less page slugs
              b_ $ with a_ [href_ $ "/" <> _channel_name ch <> "/" <> show day <> ".html"] $ do
                toHtml $ show @Text day
        Page_ChannelDay ch day logs -> do
          with article_ [class_ "post"] $ do
            h1_ $ toHtml $ _channel_name ch
            with div_ [class_ "logs"] $ do
              h2_ $ toHtml $ show @Text day
              forM_ logs $ \(Log ts s) -> do
                li_ $ do
                  let anchor = toText $ formatTime defaultTimeLocale "%H:%M:%S:%q" ts
                  with a_ [title_ $ show @Text ts, name_ anchor, href_ $ "#" <> anchor]
                    $ toHtml
                    $ formatTime defaultTimeLocale "%H:%M" ts
                  code_ $ toHtml s
  where
    stylesheet x = link_ [rel_ "stylesheet", href_ x]

-- | Define your site CSS here
pageStyle :: Css
pageStyle = "div#thesite" ? do
  C.margin (em 4) (pc 20) (em 1) (pc 20)
  ".logs" ? do
    "li" ? do
      C.listStyleType C.none
    "code" ? do
      C.fontSize $ pct 80
  "li.pages" ? do
    C.listStyleType C.none
    C.marginTop $ em 1
    "b" ? C.fontSize (em 1.2)
    "p" ? sym C.margin (px 0)

-- | Metadata in our markdown sources
data SrcMeta
  = SrcMeta
      { title :: Text,
        -- | Description is optional, hence `Maybe`
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: Source MMark -> SrcMeta
getMeta src = case MMark.projectYaml (Rib.sourceVal src) of
  Nothing -> error "No YAML metadata"
  Just val -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> toText e
    Aeson.Success v -> v
