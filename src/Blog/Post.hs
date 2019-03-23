module Blog.Post (new, toHtml, Post(..)) where

import Data.Monoid ((<>))

import Text.Pandoc
import Text.Pandoc.Walk (query)
import qualified Data.Text as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Blog.Template as Template
import qualified Blog.Comments as Comments
import qualified Blog.Sharing as Sharing
import Blog.Markdown
import Blog.Types
import Blog.Config


data Post = Post
  { getUrl :: Text.Text
  , getTitle :: Text.Text
  , getDate :: Text.Text
  , getLogo :: Text.Text
  , getHtml :: H.Html
  , getHasMath :: MathRendering
  , getReadingTime :: Int
  , getComments :: H.Html
  }


collectStr :: Inline -> [String]
collectStr (Str s) = [s]
collectStr _ = []


getDocumentIssue :: Pandoc -> Issue
getDocumentIssue (Pandoc meta _) =
  case lookupMeta "issue" meta of
    Just m -> Issue . read $ concat $ query collectStr m
    _ -> None


getDocumentTitle :: Pandoc -> Title
getDocumentTitle (Pandoc meta _) = Text.pack $ unwords $ query collectStr (docTitle meta)


getDocumentDate :: Pandoc -> Text.Text
getDocumentDate (Pandoc meta _) = Text.pack $ unwords $ query collectStr (docDate meta)


getDocumentLogo :: Pandoc -> Logo
getDocumentLogo (Pandoc meta _) =
  case lookupMeta "logo" meta of
    Nothing -> defaultIcon
    Just m -> getIcon $ concat $ query collectStr m
  where
    defaultIcon = getIcon ""

    getIcon "aws"       = "/images/aws.svg"
    getIcon "c++"       = "/images/cpp.svg"
    getIcon "ccc"       = "/images/ccc.svg"
    getIcon "hashcode"  = "/images/hashcode.png"
    getIcon "haskell"   = "/images/haskell.svg"
    getIcon "html5"     = "/images/html5.svg"
    getIcon "julia"     = "/images/julia.svg"
    getIcon "learning"  = "/images/learning.svg"
    getIcon "ocaml"     = "/images/ocaml.svg"
    getIcon "pi"        = "/images/pi.svg"
    getIcon "python"    = "/images/python.svg"
    getIcon "cliqz"     = "/images/cliqz.png"
    getIcon "adblock"   = "/images/adblocking.png"
    getIcon "raspberry" = "/images/raspberry-pi.svg"
    getIcon "v8"        = "/images/v8.svg"
    getIcon "xmonad"    = "/images/xmonad.svg"
    getIcon "synacor"   = "/images/synacor.ico"
    getIcon "linux"     = "/images/tux-logo.svg"
    getIcon _           = "/images/haskell.svg"


getApproxReadingTime :: Pandoc -> Int
getApproxReadingTime pandoc = max (textLength `div` 150) 1
  where
    textLength = length $ concatMap words $ query collectText pandoc
    collectText (Str s) = [s]
    collectText (Code _ s) = [s]
    collectText _ = []


checkIfDocumentHasMath :: Pandoc -> MathRendering
checkIfDocumentHasMath pandoc =
  if not . null . query collectMath $ pandoc
     then Enabled
     else Disabled
  where
    collectMath (Math _ _) = [True]
    collectMath _ = []

toHtml :: Post -> H.Html
toHtml post = Template.base (getTitle post) (getHasMath post) $ do
  H.h1 $ H.text (getTitle post)
  H.section H.! A.class_ "header" $ H.div $ do
    H.text $ getDate post
    H.text " | "
    H.em $ H.string $ "Reading time: ~" <> show (getReadingTime post) <> "minutes"
  H.article $ do
    getHtml post
    getComments post
  H.footer $ H.div
    H.! A.class_ "share" $ Sharing.new (blogDomain <> getUrl post) (getTitle post)
  -- TODO
  -- greenAnalytics


new :: Content -> Url -> IO Post
new content url = do
  pandoc <- runIOorExplode $ parseMarkdownToHtml content
  html <- runIOorExplode $ compileHtml pandoc

  let date = getDocumentDate pandoc
  let logo = getDocumentLogo pandoc
  let math = checkIfDocumentHasMath pandoc
  let readingTime = getApproxReadingTime pandoc
  let title = getDocumentTitle pandoc
  let issue = getDocumentIssue pandoc

  comments <- Comments.new issue

  let post = Post{
     getDate = date
   , getTitle = title
   , getLogo = logo
   , getUrl = url
   , getHtml = html
   , getHasMath = math
   , getReadingTime = readingTime
   , getComments = comments
   }

  return post
