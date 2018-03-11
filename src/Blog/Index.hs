module Blog.Index(new) where

import Data.List (sortBy, groupBy, head)
import Data.Function (on)
import qualified Data.Text as Text

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Blog.Post as Post
import qualified Blog.Template as Template
import Blog.Types (MathRendering(Disabled))


template :: H.Html -> H.Html
template html = Template.base "Posts" Disabled $ do
  html
  H.footer ""

new  :: [Post.Post] -> H.Html
new  posts = template $
  H.div H.! A.class_ "blogIndex" $ mconcat [
    indexYear ps | ps <- groupBy sameYear $ sortBy (flip compare `on` Post.getDate) posts
  ]
  where
    sameYear p1 p2
      | Text.take 4 (Post.getDate p1) == Text.take 4 (Post.getDate p2) = True
      | otherwise = False
    indexYear posts =
      H.div $ do
        H.h2 $ H.text $ Text.take 4 $ Post.getDate $ head posts
        H.ul H.! A.class_ "index" $ mconcat . map indexEntry $ posts
    indexEntry post =
      H.li $ do
        H.img H.! A.class_ "logo" H.! A.src (H.textValue $ Post.getLogo post)
        H.a H.! A.href (H.textValue $ Post.getUrl post) $ H.text $ Post.getTitle post
        H.string " - "
        H.text $ Post.getDate post
