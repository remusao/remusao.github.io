--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Contrib.LaTeX
import           Image.LaTeX.Render (imageForFormula, defaultEnv, displaymath)
import           Image.LaTeX.Render.Pandoc


--------------------------------------------------------------------------------

cssTemplateCompiler :: Compiler (Item Template)
cssTemplateCompiler = cached "Hakyll.Web.Template.cssTemplateCompiler" $
    fmap (readTemplate . compressCss) <$> getResourceString

--
main :: IO ()
main = do
    renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv
    hakyll $ do
      match "images/*" $ do
          route   idRoute
          compile copyFileCompiler

      match "css/default.css" $ compile cssTemplateCompiler
      -- match "css/*" $ do
      --     route   idRoute
      --     compile compressCssCompiler

      match (fromList ["about.md", "contact.md"]) $ do
          route   $ setExtension "html"
          compile $ pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

      match "posts/*" $ do
          route $ setExtension "html"
          compile $ (pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions
                  $ (renderFormulae defaultPandocFormulaOptions))
              >>= loadAndApplyTemplate "templates/post.html"    postCtx
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

      create ["archive.html"] $ do
          route idRoute
          compile $ do
              posts <- recentFirst =<< loadAll "posts/*"
              let archiveCtx =
                      listField "posts" postCtx (return posts) `mappend`
                      constField "title" "Archives"            `mappend`
                      defaultContext

              makeItem ""
                  >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                  >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                  >>= relativizeUrls


      match "index.html" $ do
          route idRoute
          compile $ do
              posts <- recentFirst =<< loadAll "posts/*"
              let indexCtx =
                      listField "posts" postCtx (return posts) `mappend`
                      constField "title" "Home"                `mappend`
                      defaultContext

              getResourceBody
                  >>= applyAsTemplate indexCtx
                  >>= loadAndApplyTemplate "templates/default.html" indexCtx
                  >>= relativizeUrls

      match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
