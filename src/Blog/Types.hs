module Blog.Types where

import qualified Data.Text as Text

type Title = Text.Text
type Content = Text.Text
type Url = Text.Text
type Logo = Text.Text

data MathRendering = Enabled | Disabled deriving Show
data Issue = None | Issue Int deriving Show
