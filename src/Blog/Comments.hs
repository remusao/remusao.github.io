module Blog.Comments (new) where

import GHC.Exts (toList)

import qualified System.Environment as Environment

-- Github APIs
import GitHub.Data.Definitions
import GitHub.Data.Id
import GitHub.Data.Issues
import GitHub.Data.Name
import GitHub.Data.URL (getUrl)
import qualified GitHub.Endpoints.Issues.Comments as Github

import Data.ByteString.Char8 (pack)
import Text.Pandoc
import qualified Data.Text as Text
import qualified NeatInterpolation as Interpol
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Blog.Markdown
import qualified Blog.Types as Types

data Comment = Comment
  { content :: Text.Text
  , commentAuthor :: Text.Text
  , commentUrl :: Text.Text
  , commentDate :: Text.Text
  }
  deriving (Show)

template :: Int -> [Comment] -> H.Html
template issue comments =
  H.div H.! A.class_ "comments" $ do
    H.span H.! A.class_ "commentHeader" $ do
      if not (null comments) then showIssueButton else H.string ""
      H.span H.! A.class_ "leaveComment" $
        H.a
          H.! A.href (H.stringValue issueLink)
          H.! A.title (H.stringValue issueLink)
          H.! A.target "_blank" H.! A.rel "noopener noreferrer" $ "Leave a comment on Github"
    H.ul H.! A.style "display: none;" H.! A.id "commentsList" $ mconcat $ map commentTemplate comments
  where
    showIssueButton = do
      H.button H.! A.id "showCommentsButton" H.! A.class_ "showComments" $ H.string showIssueText
      H.script $ H.toHtml ([Interpol.text|
        var button = document.getElementById('showCommentsButton');
        button.onclick = function() {
            var div = document.getElementById('commentsList');
            if (div.style.display !== 'none') {
                div.style.display = 'none';
            }
            else {
                div.style.display = 'block';
            }
        }; |])

    showIssueText =
      case length comments of
        1 -> "1 comment"
        n -> show n ++ " comments"
    issueLink = "https://github.com/remusao/remusao.github.io/issues/" ++ show issue
    commentTemplate Comment{ content, commentAuthor, commentUrl, commentDate } =
      H.li H.! A.class_ "comment" $ H.div $ do
        H.span H.! A.class_ "meta" $ do
          H.span H.! A.class_ "author" $ H.text commentAuthor
          H.string " - "
          H.a
            H.! A.href (H.textValue commentUrl)
            H.! A.title (H.textValue commentUrl)
            H.! A.target "_blank" H.! A.rel "noopener noreferrer" $ H.text commentDate
        H.div H.! A.class_ "content"$
          case runPure (markdownToHtml content) of
            Left err -> H.string (show err) -- content
            Right html -> html


stripCR :: Text.Text -> Text.Text
stripCR = Text.filter (/= '\r')


fetchComments :: Int -> IO [Comment]
fetchComments issue = do
  -- Get Github Token from env if available
  token <- Environment.lookupEnv "GITHUB_TOKEN"
  let oauth = fmap (Github.OAuth . pack) token
  print oauth

  result <- Github.comments' oauth "remusao" "remusao.github.io" (Id issue)
  case result of
    Left err -> do
      print err
      return []
    Right issues -> return . map toComment . toList $ issues
  where
    toComment IssueComment { issueCommentUser, issueCommentHtmlUrl, issueCommentCreatedAt, issueCommentBody } =
      let N user = simpleUserLogin issueCommentUser
      in Comment {
        content = stripCR issueCommentBody,
        commentAuthor = user,
        commentUrl = getUrl issueCommentHtmlUrl,
        commentDate = Text.pack $ show issueCommentCreatedAt
      }


new :: Types.Issue -> IO H.Html
new Types.None = return mempty
new (Types.Issue issue) = do
  comments <- fetchComments issue
  return $ template issue comments
