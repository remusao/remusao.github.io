module Blog.Markdown(parseMarkdownToHtml, compileHtml, markdownToHtml) where


import Text.Pandoc
import qualified Data.Text as Text
import qualified Text.Blaze.Html5 as H

extensions :: Extensions
extensions = extensionsFromList
  [ Ext_yaml_metadata_block
  , Ext_backtick_code_blocks
  , Ext_citations
  , Ext_emoji
  , Ext_fancy_lists
  , Ext_gfm_auto_identifiers
  , Ext_pipe_tables
  , Ext_implicit_figures
  , Ext_inline_notes
  , Ext_latex_macros
  , Ext_link_attributes
  , Ext_lists_without_preceding_blankline
  , Ext_markdown_in_html_blocks
  , Ext_raw_html
  , Ext_smart
  , Ext_strikeout
  , Ext_subscript
  , Ext_superscript
  , Ext_tex_math_dollars
  ]

readerOpt :: ReaderOptions
readerOpt = def
  { readerStripComments = True
  , readerStandalone = False
  , readerExtensions = extensions
  }


writerOpt :: WriterOptions
writerOpt = def
  { writerTableOfContents = True
  , writerCiteMethod = Citeproc
  , writerEmailObfuscation = JavascriptObfuscation
  , writerHTMLMathMethod = KaTeX "/katex/"
  , writerNumberOffset = [1]
  , writerNumberSections = True
  , writerTOCDepth = 1
  , writerWrapText = WrapAuto
  , writerExtensions = extensions
  }


parseMarkdownToHtml :: PandocMonad m => Text.Text -> m Pandoc
parseMarkdownToHtml = readMarkdown readerOpt


compileHtml :: PandocMonad m => Pandoc -> m H.Html
compileHtml = writeHtml5 writerOpt


markdownToHtml :: PandocMonad m => Text.Text -> m H.Html
markdownToHtml content = do
  pandoc <- parseMarkdownToHtml content
  compileHtml pandoc
