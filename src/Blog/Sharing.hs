module Blog.Sharing (new) where

import Network.URI.Encode (encodeText)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid ((<>))

import qualified Data.Text as Text
import Blog.Types

data Button = Button
  { href :: Text.Text
  , alt :: Text.Text
  , img :: Text.Text
  }


template :: [Button] -> H.Html
template sharing = H.ul $ mconcat $ map buttonTemplate $ sharing
  where
    buttonTemplate Button { href, alt, img } = H.li $ H.a
      H.! A.href (H.textValue $ encodeText href)
      H.! A.title (H.textValue alt)
      H.! A.target "_blank" H.! A.rel "noopener noreferrer" $
        H.img
          H.! A.alt (H.textValue alt)
          H.! A.src (H.textValue $ "/images/social_flat_rounded_rects_svg/" <> img)

new :: Url -> Title -> H.Html
new url title = template $ [
    Button {
        href = "https://www.facebook.com/sharer/sharer.php?u=" <> url <> "&t=" <> title
      , alt = "Share on Facebook"
      , img = "Facebook.svg"
    },
    Button {
        href = "http://twitter.com/share?url=" <> url <> "&text=" <> title <> "&via=Pythux"
      , alt = "Tweet"
      , img = "Twitter.svg"
    },
    Button {
        href = "https://getpocket.com/save?url=" <> url <> "&title=" <> title
      , alt = "Add to Pocket"
      , img = "Pocket.svg"
    },
    Button {
        href = "https://news.ycombinator.com/submitlink?u=" <> url <> "&t=" <> title
      , alt = "Submit to Hacker News"
      , img = "HackerNews.svg"
    },
    Button {
        href = "http://www.reddit.com/submit?url=" <> url <> "&title=" <> title
      , alt = "Submit to Reddit"
      , img = "Reddit.svg"
    }
  ]
