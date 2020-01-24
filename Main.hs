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
import Development.Shake (Action)
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
        _log_msg :: Msg
      }
  deriving (Eq, Show)

newtype User = User { unUser :: Text }
  deriving (Eq, Show)

data Msg 
  = Msg_Control Text
  | Msg_User User Text
  deriving (Eq, Show)

logParser :: Parser Log
logParser = do
  Just time <- fmap (posixSecondsToUTCTime . fromIntegral) . readMaybe @Int <$> some digitChar
  _ <- char ' '
  msg <- try controlMsg <|> userMsg 
  pure $ Log time msg
  where 
    controlMsg = Msg_Control <$> (string "-!-" *> restOfIt)
    userMsg = do 
      _ <- char '<'
      user <- User . toText <$> someTill anySingle (char '>')
      _ <- char ' '
      s <- restOfIt
      pure $ Msg_User user s
    restOfIt = toText <$> someTill anySingle eof

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
    groupLogs :: [Log] -> Map Day [Log]
    groupLogs = flip foldl' mempty $ \m log ->
      let k = utctDay $ _log_time log
          v = [log]
       in Map.insertWith (\new old -> old <> new) k v m

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
  | Page_User User [(Text, [Log])]

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
  -- User pages
  caleMsgs <- fmap catMaybes $ forM (Rib.sourceVal <$> chs) $ \ch -> do
    let cales = catMaybes $ flip fmap (mconcat $ Map.elems $ _channel_logs ch) $ \log@(Log _ msg) -> 
         case msg of 
           Msg_User (User "Cale") _ -> Just log
           _ -> Nothing
    if null cales 
      then pure Nothing
      else pure $ Just (_channel_name ch, cales)

  Rib.writeHtml [relfile|users/Cale.html|]
    $ renderPage 
    $ Page_User (User "Cale") caleMsgs
      
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
      Page_User (User u) _ -> do
        toHtml u
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
          forM_ (sort $ Map.keys $ _channel_logs ch) $ \day -> do
            li_ $ do
              b_ $ with a_ [href_ $ "/" <> channelDayFile ch day] $ do
                toHtml $ show @Text day
        Page_ChannelDay ch day logs -> do
          with article_ [class_ "channel"] $ do
            h1_ $ toHtml $ _channel_name ch
            h2_ $ toHtml $ show @Text day
            renderLogs LogTimeFormat_SameDay logs
        Page_User (User user) chLogs -> do 
          h1_ $ toHtml user 
          forM_ chLogs $ \(chName, logs) -> do
            h2_ $ toHtml chName
            renderLogs LogTimeFormat_Full logs

  where
    stylesheet x = link_ [rel_ "stylesheet", href_ x]

data LogTimeFormat
  = LogTimeFormat_SameDay
  | LogTimeFormat_Full
  deriving (Eq, Show)

renderLogs :: LogTimeFormat -> [Log] -> Html ()
renderLogs ltf logs = do
  with div_ [class_ "ui grid logs"] $ do
    forM_ logs $ \(Log ts msg) -> do
      with div_ [class_ "row log-message top aligned"] $ do
        with div_ [class_ $ timeColumnWidth <> " column timestamp"] $ do
          let anchor = toText $ formatTime defaultTimeLocale "%H:%M:%S" ts
          with a_ [title_ $ show @Text ts, name_ anchor, href_ $ "#" <> anchor]
            $ toHtml
            $ formatLogTime ts
        with div_ [class_ "fourteen wide column message-text"] $ case msg of 
          Msg_Control s -> with span_ [class_ "control"] $ toHtml s
          Msg_User (User user) s -> with span_ [class_ "user"] $ do 
            b_ $ toHtml user
            ": "
            toHtml s
  where
    timeColumnWidth = 
      case ltf of 
        LogTimeFormat_SameDay -> "two wide"
        LogTimeFormat_Full -> "four wide"
    formatLogTime ts = 
      case ltf of 
        LogTimeFormat_SameDay -> formatTime defaultTimeLocale "%X" ts
        LogTimeFormat_Full -> formatTime defaultTimeLocale "%F %X" ts
                        

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
  ".ui.grid.logs" ? do
    ".row.log-message" ? do
      C.important $ do 
        C.paddingTop $ em 0.5
        C.paddingBottom $ em 0
      "span.control" ? 
        C.color C.grey
      "span.user" ? 
        C.color C.black
      ".message-text" ? do
        C.fontFamily [] [C.monospace]
        C.fontSize $ pct 85
