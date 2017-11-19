---
title: IHaskell Extensions
date: 2016-01-11
logo: haskell
lang: en
---


## Introduction

In my previous [post](https://remusao.github.io/install-ihaskell-on-ubuntu-1404-with-stack.html),
I tried to provide a step-by-step explanation of how to install
[IHaskell](https://github.com/gibiansky/IHaskell) on Ubuntu 14.04 (Should also
work on other versions). Now is time to start using it!

This post is not a quickstart on how to use IHaskell, as it has already been
covered in the [official documentation](https://github.com/gibiansky/IHaskell/blob/master/notebooks/IHaskell.ipynb).
And there are more advanced examples here:

* [Conjugate Gradient](https://github.com/gibiansky/IHaskell/blob/master/notebooks/Conjugate%20Gradient.ipynb)
* [Gradient Descent](https://github.com/gibiansky/IHaskell/blob/master/notebooks/Gradient-Descent.ipynb)
* [Homophones](https://github.com/gibiansky/IHaskell/blob/master/notebooks/Homophones.ipynb)
* [Static Canvas IHaskell Display](https://github.com/gibiansky/IHaskell/blob/master/notebooks/Static%20Canvas%20IHaskell%20Display.ipynb)

Instead, I'll focus more on something a bit more mysterious for me:
*displaying custom Haskell types in notebooks*. I'll first try to give a
quick explanation on how it works, and then give basic examples of the
provided integrations with existing libraries (aeson, blaze, charts,
diagrams, etc.). I'll also try to show how to support your custom types.

**Disclaimer**: Some of the information found in the post may be
redundant with other sources (like official documentation, and in
particular the last post in the list above: *Static Canvas IHaskell
Display*), but I hope this post will bring value by giving an overview
of what is possible with *IHaskell*, explained with the words of a
newcomer. Comments and fixes would be greatly appreciated!

## How does it work?

Jupyter allows you to embed arbitrary HTML, and this mechanism is used
by IHaskell to display values in custom ways. The `IHaskellDisplay`
typeclass is used to this effect. By providing an instance for
your own types, they can be displayed in notebooks (we'll see
later that some extensions already exist to provide such display
to known Haskell libraries). A `Display` can be of several types,
but for now we will focus on `html` and `plain` (see [this notebook](https://github.com/gibiansky/IHaskell/blob/master/notebooks/Static%20Canvas%20IHaskell%20Display.ipynb) for more information).

Note that you can provide several choices of display outputs so that
your custom type can be display in notebooks *(html)* or console *(plain
text)*, the frontend will then select the best choice.


```haskell
import IHaskell.Display

-- Custom data type
data Answer = Answer

-- Make it "displayable"
instance IHaskellDisplay Answer where
    -- List of two kinds of Display: html and plain text
    display value = return $ Display [htmlDisplay, txtDisplay]
        where
            -- HTML Display
            htmlDisplay = html "<div>The answer is 42!</div>"
            -- Plain Text Display
            txtDisplay = plain "42"

-- Display an instance of our type
Answer
```


> ```sh
> The answer is 42!
> ```

In the following sections we'll see how to use some of the extensions
officially provided by *IHaskell* to display types of known libraries.
The packages we'll need are:

* ihaskell-basic
* ihaskell-aeson
* ihaskell-blaze
* ihaskell-charts
* ihaskell-diagrams
* ihaskell-magic

You can install them using stack if you intend to try this out yourself:

```sh
stack build             \
    ihaskell-basic      \
    ihaskell-aeson      \
    ihaskell-blaze      \
    ihaskell-charts     \
    ihaskell-diagrams   \
    ihaskell-magic
```

## ihaskell-basic

IHaskell [basic](https://hackage.haskell.org/package/ihaskell-basic) contains
*"Instances of IHaskellDisplay for default prelude data types"*. Currently,
only `Maybe` seems to be supported. Maybe some more *Displays* will be provided
in the future?

Anyway, here is how you can use it, and what it looks like:


```haskell
-- import IHaskell.Display.Basic
Just 42
Just (Just "Foo Bar Baz")
Nothing
```

> ```haskell
> Just 42
> Just Just "Foo Bar Baz"
> Nothing
> ```


## ihaskell-aeson

[Aeson](https://hackage.haskell.org/package/aeson) is a library used to
manipulate *JSON* format from Haskell. It allows you to use `ToJSON` and
`FromJSON` typeclasses to convert your custom data-types *to* and *from*
JSON format.

In our small example, we declare a type of document with an arbitrary
number of metadata attached, here is how we could do it:


```haskell
:extension OverloadedStrings

import qualified Data.Text as T
import qualified Data.Aeson as A
-- IHaskell.Display.Aeson

newtype Property = Property T.Text
newtype Value = Value T.Text
data Metadata = Metadata [(Property, Value)]

newtype Body = Body T.Text
newtype Title = Title T.Text
data Document  = Document
    { _title :: Title
    , _body :: Body
    , _metadata :: Metadata}

instance A.ToJSON Metadata where
   toJSON (Metadata d) = A.object $ [p A..= v | (Property p, Value v) <- d]

instance A.ToJSON Document where
   toJSON (Document (Title t) (Body b) m) = A.object [
       "title" A..= t,
       "body" A..= b,
       "metadata" A..= A.toJSON m]

document = let body = Body "Lorem Ipsum"
               title = Title "Foo Bar"
               metadata = Metadata [
                   (Property "Encoding", Value "UTF-8"),
                   (Property "Author", Value "Jonh Doe")]
           in Document {_title=title, _body=body, _metadata=metadata }

A.Null
A.Bool True
A.toJSON document
```

> ```haskell
> null
> 
> true
> 
> {
>     "body": "Lorem Ipsum",
>     "metadata": {
>         "Author": "Jonh Doe",
>         "Encoding": "UTF-8"
>     },
>     "title": "Foo Bar"
> }
> ```

## ihaskell-blaze

[Blaze](https://hackage.haskell.org/package/blaze-html) is a fast combinator library used to assemble *HTML* documents directly in Haskell code *([Embedded Domain Specific Language](https://wiki.haskell.org/Embedded_domain_specific_language))*. According to the official description, *"the project is aimed at those who seek to write web applications in Haskell – it integrates well with all Haskell web frameworks."*


```haskell
-- This example if from the IHaskell official introduction
:extension OverloadedStrings

-- import IHaskell.Display.Blaze
import Control.Monad
import Prelude hiding (div, id)
import qualified Text.Blaze.Html4.Strict as B
import qualified Text.Blaze.Html4.Strict.Attributes as A

forM [1..5] $ \size -> do
  let s = B.toValue $ size * 70
  B.img B.! A.src "https://www.google.com/images/srpr/logo11w.png" B.! A.width s
```


<img src="images/google-logo.png" width="70">
<img src="images/google-logo.png" width="140">
<img src="images/google-logo.png" width="210">
<img src="images/google-logo.png" width="280">
<img src="images/google-logo.png" width="350">


## ihaskell-charts

[Charts](https://hackage.haskell.org/package/Chart) is a *"2D charting library for haskell"*. Here a two examples taken from the [official wiki](https://github.com/timbod7/haskell-chart/wiki) on github. To adapt examples to notebooks, you must replace any reference of `toFile`, etc. by `toRenderable`.


```haskell
-- This example is taken from the wiki: https://github.com/timbod7/haskell-chart/wiki/example%205
import qualified Graphics.Rendering.Chart.Easy as E

values :: [(String, Double, Bool)]
values = [ ("Mexico City", 19.2, False)
         , ("Mumbai", 12.9, False)
         , ("Sydney", 4.3, False)
         , ("London", 8.3, False)
         , ("New York",8.2,True)]

pitem (s, v, o) = E.pitem_value E..~ v
              $ E.pitem_label E..~ s
              $ E.pitem_offset E..~ (if o then 25 else 0)
              $ E.def

E.toRenderable 
  $ E.pie_title E..~ "Relative Population"
  $ E.pie_plot . E.pie_data E..~ map pitem values
  $ E.def
```


![pie chart](/images/ihaskell-chart.svg)


```haskell
-- This example is taken from the wiki: https://github.com/timbod7/haskell-chart/wiki/example%2012
import qualified Graphics.Rendering.Chart.Easy as E

r' x y z = sqrt $ x^2 + y^2 + z^2
efield sign x y = (sign * x / r, sign * y / r) where r = r' x y 10
bfield sign x y = (-sign * y / r^2, sign * x / r^2) where r = r' x y 10
square a s = [(x, y) | x <- range, y <- range] where range = [-a, -a + s..a] :: [Double]
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

ef (x, y) = efield 1 (x - 20) y `add` efield (-1) (x + 20) y
bf (x, y) = bfield 1 (x - 20) y `add` bfield (-1) (x + 20) y
grid = square 30 3

vectorField title f grid = fmap E.plotVectorField $ E.liftEC $ do
    c <- E.takeColor  
    E.plot_vectors_mapf E..= f
    E.plot_vectors_grid E..= grid
    E.plot_vectors_style . E.vector_line_style . E.line_color E..= c
    E.plot_vectors_style . E.vector_head_style . E.point_color E..= c
    E.plot_vectors_title E..= title

main = E.toRenderable $ do
    E.setColors [E.opaque E.black, E.opaque E.blue]

    E.layout_title E..= "Positive and Negative Charges"
    E.plot $ vectorField "Electric Field" ef grid
    E.plot $ vectorField "B-field" bf grid
main
```

![field](/images/ihaskell-field.svg)


## ihaskell-diagrams

[Diagrams](http://projects.haskell.org/diagrams/) is a *"powerful, flexible, declarative domain-specific language for creating vector graphics"*. That is, it provides a flexible embedded DSL used to describe vectorized figures that you can then render using different backends.


```haskell
:extension NoMonomorphismRestriction
:extension FlexibleContexts
:extension GADTs

import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Sunburst as S
import Data.Tree (unfoldTree)

aTree = unfoldTree (\n -> (0, replicate n (n - 1))) 6
diagram $ S.sunburst aTree D.# D.centerXY D.# D.pad 1.1
```

![field](/images/ihaskell-diagram1.svg)


```haskell
-- import Diagrams.Backend.SVG.CmdLine as 
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Factorization as F

diagram $ F.fdGridList 6 D.# D.center D.# D.pad 1.05
```


![field](/images/ihaskell-diagram2.svg)


## ihaskell-magic

In this case, libmagic is used to determine type of files using magic
values at the beginning, which allows us to display binary content in
notebooks, in the right way. For example, if we want to display an
image (png, jpeg, svg, etc.), we just have to read its content as a
`ByteString`, then `libmagic` is used behind the scene by *IHaskell* to
determine what kind of content it is, based on the magic bytes present
in the file, and then inline it in the notebook.

What is important to understand here, is that an instance of
IHaskellDisplay is defined for both `ByteString` and `Text`, in case
this strings represent something else than text (images, for example),
*IHaskell* is able to know it thanks to `libmagic` and display the content
accordingly, otherwise, it just displays the string.


```haskell
import qualified Data.ByteString as B
B.readFile "./haskell-logo.png"
```


![Haskell logo](/images/output_25_0.png)



```haskell
:extension OverloadedStrings
import qualified Data.ByteString as B
"This is a string of type ByteString" :: B.ByteString
```


    This is a string of type ByteString



```haskell
:extension OverloadedStrings
import qualified Data.Text as T
"This is a string of type Text" :: T.Text
```


    This is a string of type Text


## ihaskell-static-canvas

Following on the [excellent introduction](https://github.com/gibiansky/IHaskell/blob/master/notebooks/Static%20Canvas%20IHaskell%20Display.ipynb)
from the author of *IHaskell*. Let's try to use
[static-canvas](https://github.com/jeffreyrosenbluth/static-canvas) to
create more elaborate inlings in notebooks! The github page is full
of [examples]() that we can try out right now. But before that, we
must create an instance of `IHaskellDisplay` for `CanvasFree` (which
is taken directly from the mentionned tutorial (I hope it's Ok, since
`ihaskell-static-canvas` doesn't seem to be part of Stackage LTS at the
moment).


```haskell
import IHaskell.Display -- From the 'ihaskell' package.
import IHaskell.IPython.Types (MimeType(..))
import Graphics.Static  -- From the 'static-canvas' package.

-- Text conversion functions.
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
```


```haskell
-- Since CanvasFree is a type synonym, we need a language pragma.
:extension TypeSynonymInstances
:extension FlexibleInstances

instance IHaskellDisplay (CanvasFree ()) where
  -- display :: CanvasFree () -> IO Display
  display canvas = return $
    let src = toStrict
      $ toLazyText
      $ buildScript width height canvas
    in Display [DisplayData MimeHtml src]
    where (height, width) = (200, 600)
```

Now let's try some example:


```haskell
import Graphics.Static
import Graphics.Static.ColorNames

text :: CanvasFree ()
text = do
  font "italic 60pt Calibri"
  lineWidth 6
  strokeStyle blue
  fillStyle goldenrod
  textBaseline TextBaselineMiddle
  strokeText "Haskell" 150 100 
  fillText "Haskell!" 150 100

text
```

![](/images/ihaskell-canvas.png)


## Conclusion

This was a short introduction without much new stuff, but it gave me a
better understanding on how *IHaskell* (and in a way, *Jupyter*) works.
Thanks to the awesome work of some haskellers, we are able to benefit
from the great *Jupyter* ecosystem, and I think it can bring a lot to
*Haskell* itself. It's easier to share code, easier to write about
*Haskell*-related stuff, easier to dig into a new projet.

*IHaskell* is a solid foundation for more to come: more widgets, more
integrations with *Haskell* libraries, etc. I wonder if, for example, we
could use *Blaze* to generate *Display* on-the-fly? Could we reuse some
code from the *Python* Kernel of *Jupyter*? I'm also looking forward to
try the `ihaskell-widgets` extension.

That's pretty much it, I'd like to say I'm very excited for Haskell,
because it becomes much more accessible for newcomers, thanks to (for
example) Stack, IHaskell, and lot of effort that is being made by the
community. I hope I can continue to contribute at my level to this
effort!

Thanks for reading!
