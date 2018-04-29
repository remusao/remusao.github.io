{-
TODO:
- Check front end checklist: https://github.com/thedaviddias/Front-End-Checklist
- Add automatic check with https://validator.w3.org/nu/?doc=

- Add fully static search
  https://github.com/LeaVerou/awesomplete
  https://github.com/algolia/autocomplete.js

- create 404 and 5xx pages
- Add green analytics
- Add RSS feed
- Add table of content
- Improve syntax highlighting
- Improve styling
- Improve code
-}

import qualified Blog
import qualified System.Environment as Environment

main :: IO ()
main = do
  programArgs <- Environment.getArgs
  case programArgs of
    ["serve"] -> Blog.serve
    _         -> Blog.build
