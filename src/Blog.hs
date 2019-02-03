module Blog where

import qualified Data.Map as M

import Control.Concurrent.Chan (Chan, getChanContents, newChan)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Monad (foldM, foldM_)
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock as Time
import System.FilePath (takeBaseName, takeExtension)
import "filemanip" System.FilePath.Glob (namesMatching)
import Text.Printf (printf)
import qualified Data.Text as Text
import Data.Monoid ((<>))

import Control.Concurrent.Async (concurrently_)
import qualified Text.Blaze.Html.Renderer.String as B

import System.FSNotify

import qualified Blog.Index as Index
import qualified Blog.Post as Post
import qualified Blog.Serve as Serve


-- Keep track of the current posts in the blog
type Blog = M.Map FilePath Post.Post

getFilePath :: Event -> FilePath
getFilePath (Added f _ _) = f
getFilePath (Modified f _ _) = f
getFilePath (Removed f _ _) = f

isFileAltered :: Event -> Bool
isFileAltered Added{} = True
isFileAltered Modified{} = True
isFileAltered _ = False

updateSite :: (String -> IO ()) -> Blog -> [Event] -> IO Blog
updateSite logging blog newFiles
  -- TODO: Parallel for to produce [Post]
  -- TODO: (then) Sequential fold to update the Map
 = do
  updatedBlog <- foldM updateBlog blog newFiles
  let posts = M.elems updatedBlog
  writeFile "index.html" $ B.renderHtml $ Index.new posts
  logging $ printf "Generating index.html for %i posts..." (length posts)
  return updatedBlog
  where
    updateBlog :: Blog -> Event -> IO Blog
    updateBlog b event
      | isFileAltered event = do
        let f = getFilePath event
        let name = takeBaseName f
        logging $ printf "Generating %s..." f
        content <- TIO.readFile f

        let output = "posts/" <> Text.pack name <> ".html"
        post <- Post.new content output
        writeFile (Text.unpack output) $ B.renderHtml $ Post.toHtml post

        let outputLegacy = "posts/" <> Post.getDate post <> "-" <> Text.pack name <> ".html"
        writeFile (Text.unpack outputLegacy) $ B.renderHtml $ Post.toHtml post

        return $
          M.alter
            (\case
               Nothing -> Just post
               Just _ -> Just post)
            name
            b
      | otherwise = do
        let name = takeBaseName (getFilePath event)
        return $
          M.alter
            (\case
               Nothing -> Nothing
               Just _ -> Nothing)
            name
            b

generateSite :: (String -> IO ()) -> Blog -> Chan Event -> IO ()
generateSite logging blog fileModifiedEvents = do
  events <- getChanContents fileModifiedEvents
  foldM_ (\c e -> updateSite logging c [e]) blog events

mkLogger :: IO (String -> IO ())
mkLogger
  -- Thread-safe logging function
 = do
  lock <- newMVar ()
  return $ \str -> withMVar lock (\_ -> putStrLn str)

initialBuild :: (String -> IO ()) -> IO Blog
initialBuild logging
  -- Create list of posts
 = do
  now <- Time.getCurrentTime
  files <- namesMatching "./posts/*.md"
  -- Init blog with all posts
  updateSite logging M.empty $ fmap (\f -> Added f now False) files

build :: IO ()
build = do
  logging <- mkLogger
  _ <- initialBuild logging
  putStrLn "Done."

serve :: IO ()
serve = do
  logging <- mkLogger
  -- Init blog with all posts
  blog <- initialBuild logging
  fileModifiedEvents <- newChan
  -- Watch tree directory
  concurrently_ (generateSite logging blog fileModifiedEvents) $
    withManager $ \mgr
      -- start a watching job (in the background)
     -> do
      _ <-
        watchTreeChan
          mgr -- manager
          "./posts/" -- directory to watch
          (\e -> takeExtension (getFilePath e) == ".md") -- predicate
          fileModifiedEvents
      putStrLn "listening..."
      Serve.serve logging
