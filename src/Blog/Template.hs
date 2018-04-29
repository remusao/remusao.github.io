module Blog.Template (base) where

import Data.Monoid ((<>))
-- import NeatInterpolation (text)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Blog.Css
import Blog.Types
import Blog.Config

-- greenAnalytics :: H.Html
-- greenAnalytics =
--   H.span $ do
--     H.script $ H.toHtml ([text|
--       var conf = {
--           siteID: ''
--       };
--
--       window.onload = function() {
--           if (navigator.doNotTrack H.!= '1') {
--               var urlDetails = {
--                   origin: document.location.host,
--                   protocol: document.location.protocol,
--                   href: document.location.href,
--                   ref: document.referrer
--               };
--
--               conf.urlDetails = urlDetails;
--               var probeWindow = document.getElementById("green-analytics").contentWindow;
--               probeWindow.postMessage(conf, 'https://vhizszxi.herokuapp.com/collect');
--           }
--       }; |])
--     H.iframe
--       H.! A.id "green-analytics"
--       H.! A.style "display: none"
--       H.! A.src "https://vhizszxi.herokuapp.com/frame" $ ""



-- TODO: Change the language of the page depending on the post
-- TODO: open graph tags (Facebook + Twitter)
base :: Title -> MathRendering -> H.Html -> H.Html
base title math post = H.docTypeHtml H.! A.lang "en" H.! A.dir "ltr" $ do
  H.head $ do
    -- Set character encoding for the document
    H.meta H.! A.charset "utf-8"
    -- Instruct Internet Explorer to use its latest rendering engine
    H.meta H.! A.httpEquiv "x-ua-compatible" H.! A.content "ie=edge"
    -- Viewport for responsive web design
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
    -- Meta Description
    H.meta H.! A.name "description" H.! A.content (H.textValue blogDescription)
    -- Declare favicon TODO: use png + 32x32px
    H.link H.! A.rel "icon" H.! A.type_ "image/x-icon" H.! A.href "/images/favicon.ico"
    -- Apple Touch Icon TODO: use png + 200x200px
    H.link H.! A.rel "apple-touch-icon" H.! A.href "/images/favicon.ico"

    H.title $ H.text $ blogTitle <> " - " <> title

    -- Inline CSS
    css

    case math of
      Disabled -> ""
      Enabled -> do
        H.script H.!
          A.src ("/katex/katex.min.js") $ mempty
        H.script H.!
          A.src ("/katex/contrib/auto-render.min.js") $ mempty
        H.script "document.addEventListener(\"DOMContentLoaded\", function() {\n  renderMathInElement(document.body);\n});"
        H.link H.! A.rel "stylesheet" H.!
          A.href ("/katex/katex.min.css")
  H.body $ do
    H.header $ H.h1 $ H.a H.! A.href "../" $ H.span "Simplex Sigillum Veri"
    H.main post
