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

      match "posts/*" $ do
          route $ setExtension "html"
          compile $
            pandocCompilerWithTransformM defaultHakyllReaderOptions
            defaultHakyllWriterOptions
            (renderFormulae defaultPandocFormulaOptions)
              >>= loadAndApplyTemplate "templates/post.html" postCtx
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

      match "index.html" $ do
          route idRoute
          compile $ do
              posts <- recentFirst =<< loadAll "posts/*"
              let indexCtx =
                      listField "posts" postCtx (return posts) `mappend`
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
