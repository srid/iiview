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

-- | Parse the channel name out of its "out" filename (eg: "#nixos/")
channelDirNameParser :: Parser Text
channelDirNameParser = do
  _ <- some (char '#')
  toText <$> someTill printChar (char '/')

data Channel
  = Channel
      { _channel_name :: Text,
        _channel_logs :: [Log]
      }
  deriving (Eq, Show)

data Log
  = Log
      { _log_text :: Text
      }
  deriving (Eq, Show)

parseChannel :: (MonadIO m, MonadFail m) => Path b File -> m (Either Text Channel)
parseChannel fp = do
  let chDirName = toText $ toFilePath $ dirname $ parent fp
  case parse channelDirNameParser "" chDirName of
    Left e ->
      pure $ Left $ toText $ errorBundlePretty e
    Right chName -> do
      logs <- fmap Log . lines <$> readFileText (toFilePath fp)
      pure $ Right $ Channel chName logs

-- | This will be our type representing generated pages.
--
-- Each `Source` specifies the parser type to use. Rib provides `MMark` and
-- `Pandoc`; but you may define your own as well.
data Page
  = Page_Index [Source Channel]
  | Page_Single (Source Channel)

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
    Rib.forEvery [[relfile|#*/out|]] $ \src -> do
      Rib.buildHtml' src parseChannel channelOutFile $ renderPage . Page_Single
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
      Page_Single ch -> do
        toHtml $ _channel_name $ Rib.sourceVal ch
        " - ii viewer"
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    with div_ [class_ "ui text container", id_ "thesite"] $ do
      case page of
        Page_Index chs -> div_ $ forM_ chs $ \ch ->
          with li_ [class_ "pages"] $ do
            b_ $ with a_ [href_ $ Rib.sourceUrl ch] $ do
              toHtml $ _channel_name $ Rib.sourceVal ch
        Page_Single (Rib.sourceVal -> ch) ->
          with article_ [class_ "post"] $ do
            h1_ $ toHtml $ _channel_name ch
            with div_ [class_ "logs"]
              $ forM_ (_channel_logs ch)
              $ \(Log logS) -> do
                li_ $ code_ $ toHtml logS
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
