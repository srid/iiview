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
import Control.Monad.Except (liftEither, withExceptT)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Time
import Data.Time.Clock.POSIX
import Development.Shake
import Lucid
import Path
import Rib (Source)
import qualified Rib
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (div, some)

type Parser = Parsec Void Text

data Log
  = Log
      { _log_time :: UTCTime,
        _log_msg :: Text
      }
  deriving (Eq, Show)

logParser :: Parser Log
logParser = do
  Just time <- fmap (posixSecondsToUTCTime . fromIntegral) . readMaybe @Int <$> some digitChar
  -- TODO: parse msg further
  msg <- toText <$> someTill anySingle eof
  pure $ Log time msg

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

mkChannel :: Text -> [Log] -> Channel
mkChannel chName = Channel chName . groupLogs
  where
    groupLogs =
      fmap (sortOn _log_time)
        . Map.fromListWith (<>)
        . fmap (\l -> (utctDay $ _log_time l, [l]))

-- Parse ii's channel "out" file
parseChannelFile :: (MonadIO m, MonadFail m) => Path b File -> m (Either Text Channel)
parseChannelFile fp = runExceptT $ withExceptT (toText . errorBundlePretty) $ do
  let chDirName = toText $ toFilePath $ dirname $ parent fp
  chName <- liftEither $ parse channelDirNameParser "" chDirName
  logsRaw <- lines <$> readFileText (toFilePath fp)
  logs <- forM logsRaw $ liftEither . parse logParser ""
  pure $ mkChannel chName logs

data Page
  = Page_Index [Source Channel]
  | Page_Channel (Source Channel)
  | Page_ChannelDay Channel Day [Log]

main :: IO ()
main = Rib.run [reldir|a|] [reldir|b|] generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  Rib.buildStaticFiles [[relfile|static/**|]]
  chs <-
    Rib.forEvery [[relfile|#*/out|]] $ \fp -> do
      chSrc <- Rib.buildHtml' fp parseChannelFile channelOutFile $ renderPage . Page_Channel
      let ch = Rib.sourceVal chSrc
      forM_ (Map.toList $ _channel_logs ch) $ \(day, logs) -> do
        dayFp <- liftIO $ parseRelFile $ toString $ channelDayFile ch day
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

-- TODO: refactor `Source` so we can use it for source-less page slugs
channelDayFile :: Channel -> Day -> Text
channelDayFile ch day = _channel_name ch <> "/" <> show day <> ".html"

-- | HTML view of our site.
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
              b_ $ with a_ [href_ $ "/" <> channelDayFile ch day] $ do
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
