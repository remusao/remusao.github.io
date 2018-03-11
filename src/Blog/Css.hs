module Blog.Css (css) where

import Stitch
import Stitch.Render
import Text.Blaze.Html5
import qualified NeatInterpolation as Interpol

cssFont :: Html
cssFont = style $ text $ [Interpol.text|
@font-face {
  font-family: 'Inconsolata';
  font-style: normal;
  font-weight: 400;
  src: local('Inconsolata Regular'), local('Inconsolata-Regular'), url(/fonts/Inconsolata.woff2) format('truetype');
}

@font-face {
  font-family: 'Open Sans';
  font-style: italic;
  font-weight: 400;
  src: local('Open Sans Italic'), local('OpenSans-Italic'), url(/fonts/OpenSans_italic_400.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2212, U+2215;
}

@font-face {
  font-family: 'Open Sans';
  font-style: italic;
  font-weight: 700;
  src: local('Open Sans Bold Italic'), local('OpenSans-BoldItalic'), url(/fonts/OpenSans_italic_700.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2212, U+2215;
}

@font-face {
  font-family: 'Open Sans';
  font-style: normal;
  font-weight: 400;
  src: local('Open Sans Regular'), local('OpenSans-Regular'), url(/fonts/OpenSans_normal_400.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2212, U+2215;
}

@font-face {
  font-family: 'Open Sans';
  font-style: normal;
  font-weight: 700;
  src: local('Open Sans Bold'), local('OpenSans-Bold'), url(/fonts/OpenSans_normal_700.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2212, U+2215;
}
|]


-- TODO: Make responsive and test for:
-- 320px, 768px, 1024px
cssBase :: Html
cssBase = style $ text $ renderCSSWith compressed $
  -- "div.blogIndex" ? do
  --   "padding" .= "0"
  --   "margin" .= "0"

  -- Style body
  "body" ? do
    -- Text style and general layout
    "background" .= "#ffffff"
    "color" .= "rgba(0,0,0,.84)"

    -- "font-family" .= "georgia,Arial,'Helvetica Neue','Helvetica',sans-serif"
    "font-family" .= "'Open Sans', sans-serif"
    "font-size" .= "17px"
    "line-height" .= "1.6em"
    "text-rendering" .= "optimizeLegibility"

    "margin" .= "0 auto"
    "max-width" .= "40em"
    "padding" .= "0 0.5em"

    -- Style Blog Header
    "header" ? do
      "border-bottom" .= "thin solid #d3d7cf"
      "margin-bottom" .= "15px"
      "padding-bottom" .= "30px"
      "padding-top" .= "25px"

      -- margin-bottom: 30px;
      -- padding: 12px 0px 12px 0px;

      "h1" ? do
        "font" .= "inherit"
        "font-size" .= "2.5em"
        "line-height" .= "2.5em"
        "text-align" .= "center"

      "a" ? do
        "color" .= "#63007d"
        "text-decoration" .= "none"
        "display" .= "block"
        "border" .= "none"

    "footer" ? do
      "border-top" .= "thin solid #d3d7cf"
      "color" .= "#555"
      "font-size" .= "12px"
      "margin-top" .= "30px"
      "padding" .= "12px 0px 12px 0px"

    -- Style Blog Content
    "main" ? do
      -- Style sharing buttons
      "div.share" ? do
        "ul" ? do
          "padding" .= "0 0 0 1em"
          "padding-left" .= "0"
          "text-align" .= "right"

          "img" ? do
            "width" .= "2.5em"

          "li" ? do
            "display" .= "inline"
            "margin" .= "0px 5px"

      "img.logo" ? do
        "width" .= "1em"
        "height" .= "1em"
        "display" .= "inline"
        "margin" .= "0px 1em"
        "opacity" .= "0.6"

      "section.header" ? do
        "color" .= "#555"
        "font-style" .= "italic"
        "display" .= "block"

      "h1" ? do
        "font" .= "inherit"
        "font-size" .= "1.7em"
        "line-height" .= "1.5em"

      "ul" ? do
        "margin" .= "1.0em 0"
        "padding" .= "0 0 0 1em"
        "padding-left" .= "2em"

        "li" ? do
          "margin" .= "5px 0"

      "ul.index" ? do
        "list-style" .= "none"
        "margin-top" .= "1em"
        "margin-bottom" .= "1em"
        "margin-left" .= "0"
        "margin-right" .= "0"
        "padding-left" .= "0"

      "div.comments" ? do
        "margin-top" .= "2rem"
        "padding" .= "16px 16px 16px 14px"
        "line-height" .= "1.2rem"
        "font-size" .= "15px"

        ".showComments" ? do
          "background-color" .= "Transparent"
          "background-repeat" .= "no-repeat"
          "border" .= "none"
          "outline" .= "none"
          "overflow" .= "hidden"
          "color" .= "#516AE8"
          "font-weight" .= "bold"
          "cursor" .= "pointer"
          "height" .= "32px"
          "margin-right" .= "2em"

        ".leaveComment" ? do
          "a" ? do
            "-webkit-appearance" .= "button"
            "-moz-appearance" .= "button"
            "appearance" .= "button"

            "background-color" .= "#8f4e8b"
            "box-shadow" .= "0 1px 3px rgba(0,0,0,0.24)"
            "padding" .= "2px 18px"
            "background-color" .= "#8f4e8b"
            "outline" .= "none"
            "border" .= "none"
            "color" .= "#fff !important"
            "cursor" .= "pointer"
            "height" .= "32px"

          "a:hover" ? do
            "box-shadow" .= "0 1px 20px rgba(0,0,0,0.24)"

        "ul" ? do
          "list-style" .= "none"

        "li.comment" ? do
          "border-radius" .= "2px"
          "box-shadow" .= "0 1px 5px rgba(0,0,0,0.16)"
          "display" .= "block"
          "background-color" .= "#fafbfd"
          "margin" .= "1em 0"
          "padding" .= "5px 5px 5px 5px"

          ".author" ? do
            "font-weight" .= "bold"

          ".meta" ? do
            "line-height" .= "2em"
            "padding-bottom" .= "5px"
            "border-bottom" .= "thin solid #d3d7cf"

          ".content" ? do
            "padding-left" .= "20px"
            "padding-right" .= "10px"

      ".index" ? do
        "color" .= "#918d8d"
        "display" .= "inline"

        "a" ? do
          "color" .= "#2e3436"
          "text-decoration" .= "none"

      "article" ? do
        "p" ? do
          "hyphens" .= "auto"

        "a" ? do
          "color" .= "#3465a4"
          "text-decoration" .= "none"

        "a:visited" ? do
          "color" .= "#75507b"

        "h2" ? do
          "font-size" .= "1.3em"
          "line-height" .= "1.2em"
          "margin" .= "1em 0px 0px 0px"

        ".figure" ? do
          "text-align" .= "center"

        "figcaption" ? do
          "text-align" .= "center"
          "font-style" .= "italic"
          "color" .= "#696f72"
          "line-height" .= "2em"

        "img" ? do
          "display" .= "block"
          "margin-left" .= "auto"
          "margin-right" .= "auto"
          "text-align" .= "center"

        "blockquote" ? do
          "border-left" .= "thin solid #d3d7cf"
          "margin" .= "10px"
          "padding" .= "0 0 0 1em"
          "color" .= "#555555"

      -- Style source code
        "a.sourceLine" ? do
          "color" .= "rgba(0,0,0,.84)"


        "code" ? do
          "font-family" .= "Inconsolata"
          "background-color" .= "#f3f4f5"
          "font-size" .= "17px"
          "line-height" .= "1.2em"

        "code.sourceCode" ? do
          "overflow-x" .= "auto" -- Make code block scrollable
          "display" .= "block"
          "padding" .= "8px"
          "margin-bottom" .= "1em"

          -- Colors
          ".al" ? do { "color" .= orange; "font-weight" .= "bold"; }   -- Alert
          ".an" ? do { "color" .= violet; "font-weight" .= "bold"; }   -- Annotation
       -- ".at" ? do }                                                    -- Attribute
          ".av" ? do { "color" .= orange; }
          ".bn" ? do { "color" .= cyan; }                            -- BaseNTok
          ".cf" ? do { "color" .= violet; }                            -- Control Flow
          ".ch" ? do { "color" .= cyan; }                            -- Char
          ".co" ? do { "color" .= base01; "font-style" .= "italic"; }  -- CommentTok
          ".cv" ? do { "color" .= orange; }                            -- Comment Var
          ".dt" ? do { "color" .= yellow; }                            -- DataType
          ".dv" ? do { "color" .= cyan; }                            -- Decimal Value
          ".er" ? do { "color" .= red; "font-weight" .= "bold"; }   -- Error
          ".ex" ? do { "color" .= orange; }                            -- Extension
          ".fl" ? do { "color" .= cyan; }                            -- Float
          ".fu" ? do { "color" .= blue; }                            -- Function
          ".im" ? do { "color" .= blue; }                            -- Import
          ".kw" ? do { "color" .= green; "font-weight" .= "bold"; }   -- KeyWord
          ".op" ? do { "color" .= orange; }                            -- Builtin
          ".op" ? do { "color" .= orange; }                            -- Operator
          ".ot" ? do { "color" .= orange; }                            -- Other
       -- ".re" ? do }                                                    -- RegionMarker
          ".sc" ? do { "color" .= cyan; "font-weight" .= "bold"; }  -- Special Char
          ".ss" ? do { "color" .= cyan; }                            -- Verbatim String
          ".st" ? do { "color" .= cyan; }                            -- String
          ".va" ? do { "color" .= orange; }                            -- Variable
          ".vs" ? do { "color" .= cyan; }                            -- Special String
          ".wa" ? do { "color" .= red; "font-weight" .= "bold"; }  -- Warning
  where
    -- base0 =   "#839496"
    -- base00 =  "#657b83"
    -- base02 =  "#073642"
    -- base03 =  "#002b36"
    -- base1 =   "#93a1a1"
    -- base2 =   "#eee8d5"
    -- base3 =   "#fdf6e3"
    -- magenta = "#d33682"
    base01 =  "#586e75"
    blue =    "#268bd2"
    cyan =    "#2aa198"
    green =   "#859900"
    orange =  "#cb4b16"
    red =     "#dc322f"
    violet =  "#6c71c4"
    yellow =  "#b58900"


css :: Html
css = do
  cssFont
  cssBase
