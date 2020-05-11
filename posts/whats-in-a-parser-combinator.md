---
title: What's in a parser combinator?
date: 2016-02-23
logo: haskell
lang: en
---

As part of my ongoing effort to make progress in Haskell (that's
one of my goals for 2016!), I'm following the [MOOC on functionnal programming](https://courses.edx.org/courses/course-v1:DelftX+FP101x+3T2
015/info) by [Erik Meijer](https://twitter.com/headinthebox) on *edX*.

The first lessons were pretty basic stuff, and I got through
them quickly. Lesson 7 is about *Functional parsers* and
M\*\*\*\*\* (scary). This is where I encountered my first
difficulties, and I thought it would be an interesting writing.
I already used parser combinators in Haskell before (mainly
[Parsec](https://hackage.haskell.org/package/parsec) and
[Attoparsec](http://hackage.haskell.org/package/attoparsec)), but never
really understood how they worked, or at least not enough to implement
one myself. So here is my take on the subject. Don't expect really
advanced stuff! It's just an introduction to the basic concepts, on
which we could build more complex and useful tools. In particular, **I
won't talk about**:

1. How to report errors.
2. How to recover from errors.
3. How to write a parser for a concrete grammar.

Instead **I'll focus on**:

1. What a parser *is*.
2. How to make parsers *compose*.
3. How to use *do notation* to implement more complex parsers.

One of the interesting facts about writing your own parser combinators
library, is that you will learn (or consolidate) other knowledges in
the process, like: *Functors*, *Applicatives* and, of course, *Monads*,
and more generaly, how to *design DSL in Haskell*. I already knew about
this concepts (at least, that's what I thought...), but knowing what
something is from a high level of abstraction, *is not the same as
knowing how to implement it on a concrete type* (like a Parser)!

### So what's a parser?

We can view a *Parser* as *something* that consumes some input, and
outputs a structured representation of what was consumed. For the sake
of simplicity, we'll only consume strings (Haskell type `String`). So
that would be something like:


```haskell
type Parser a = String -> a
```

Here `a` represents the type of what is *built* from the stream of
characters (`String`). This could be a syntactic tree, or a list
of numbers, or anything else. For example a parser that is able to
recognize a string like `"[1, 2, 3, 4]"` could have the type: `Parser [Int]`
(expended to `String -> [Int]`), which means it takes a `String`
and output a `list` of integers.

But we're missing two important properties of a *Parser*:

1. It can **fail to parse** something.
2. It can **partially consume** its input.

To take into account the first point, we could return `Maybe a` instead
of `a` (resulting in `Nothing` in case of failure). Note that we could
also use a richer type like `Either` to handle parsing errors. And for
the second point, we can return a tuple of a `a` and a `String`, which
represents the part of the string that wasn't consumed by the parser.
The type would then become:


```haskell
data Parser a = Parser { runParser :: String -> Maybe (a, String) }
```

As an example of a parser that would fail, if you take our previous
*parser* that is able to handle a list of integers, if you give it the
string `"[1 ,2"`, it will fail, and return `Nothing`.

Similarly, if we feed the *parser* with `"[1, 2, 3, 4]toto"`, it will
consume the part of the string that represents the list of integers, and
leave `"toto"` as a remaining input. Thus the result would be: `Just
([1, 2, 3, 4], "toto")`.

Let's implement some very basic parsers:


```haskell
-- This parser always fails
failure :: Parser a
failure = Parser $ \s -> Nothing
```


```haskell
-- This parser always succeeds and returns the value given as input
-- (leaving the input string intact)
return :: a -> Parser a
return a = Parser $ \s -> Just (a, s)
```


```haskell
-- This parser returns the first char of the input string, and
-- fail on empty input
oneChar :: Parser Char
oneChar = Parser $ \s -> case s of
            [] -> Nothing
            (c:xs) -> Just (c, xs)
```

Let's test these parsers:


```haskell
runParser failure "Hello Parser!"
```

> ```haskell
> Nothing
> ```


```haskell
runParser (return 42) "Hello Parser!"
```

> ```haskell
> Just (42, "Hello Parser!")
> ```


```haskell
runParser oneChar "Hello Parser!"
```

> ```haskell
> Just ('H',"ello Parser!")
> ```


```haskell
runParser oneChar ""
```

> ```haskell
> Nothing
> ```


The basic parsers seem to behave as expected. We get `Nothing` in case
of failure, and they are able to partially consume the input. So all
is good, but what about more complex parsers? We would like to parse
strings, or more complex patterns. Let's try to recognize a string from
the input, using our basic parsers:


```haskell
string :: String -> Parser String
string "" = return ""
string (c1:xs1) = Parser $ \s ->
  case runParser oneChar s of
    Nothing -> Nothing
    Just (c2, rest) ->
      if c1 == c2
      then case runParser (string xs1) rest of
        Nothing -> Nothing
        Just (match, rest2) -> Just (c2:match, rest2)
      else Nothing
```


```haskell
runParser (string "Hello") "Hello Parser!"
```

> ```haskell
> Just ("Hello", " Parser!")
> ```


```haskell
runParser (string "Hello") "Foo Bar"
```

> ```haskell
> Nothing
> ```


```haskell
runParser (string "") "Hello Parser!"
```

> ```haskell
> Just ("","Hello Parser!")
> ```


This isn't very convenient (but it works)... Because we have to write
the boilerplate to *compose parsers* over and over. Hopefully, we know a
famous structure that allows composition in Haskell, and this is called
*Monad* (and I won't make yet another tutorial on *Monads*, so I will
assume you already are familiar with this concept). That means we could
avoid all the boilerplate, by making our `Parser` type an instance of
*Monad*. This would allow us to use the *do syntax* to cleanly compose
our parsers! Sweet!

To do so, we'll have to make our *Parser* an instance of: *Functor*,
*Applicative* and *Monad*.

#### Parser is a Functor

First of all, our Parser is an instance of [*Functor*](https://en.wikibooks.org/wiki/Haskell/The_Functor_class),
which means we can `map` functions over the result of our parsing:


```haskell
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    -- 1. Run parser on input string.
    -- 2. Apply function on result of parsing.
    fmap f p = Parser $ \s ->
      case runParser p s of
        Nothing -> Nothing
        Just (a, rest) -> Just (f a, rest)
```


```haskell
-- Parse `String` "42" and then convert it to `Int` using `read`
parse42 :: Parser Int
parse42 = (fmap read $ string "42")

runParser parse42 "42 is the answer!"
```

> ```haskell
> Just (42, " is the answer!")
> ```


#### Parser is an Applicative

Secondly, we can make our parser an instance of
[*Applicative*](https://en.wikibooks.org/wiki/Haskell/Applicative_functors).
This part wasn't obvious for me. All the examples I found were
about instances for easy types like `Maybe`, but I found a *Parser* to
be pretty different. But thanks to the types and some use-cases (that
you'll find below), I figured the following implementation (which will
hopefully be correct...):


```haskell
instance Applicative Parser where
    -- pure :: a -> Parser a
    -- Wrap a value inside a parser, leaving input unchanged.
    pure a = Parser $ \s -> Just (a, s)
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    -- 1. Run first parser on input (resulting in a function (a -> b).
    -- 2. Run second parser on remaining input, left by first parser.
    -- 3. Apply function (a -> b) on result of second parser.
    p1 <*> p2 = Parser $ \s ->
      case runParser p1 s of
        Nothing -> Nothing
        Just (f, rest) -> case runParser p2 rest of
          Nothing -> Nothing
          Just (a, rest2) -> Just (f a, rest2)
```

The usefulness of the previous instance might not be obvious, but it
allows us to `lift` some function inside the realm of parsers. For
example if we want to take the result of several parsers and then group
their results into a tuple, we can do it using *Applicatives*:


```haskell
parseTuple :: Parser (Char, Char)
parseTuple =  (,) <$> oneChar <*> oneChar
runParser parseTuple "ab"
```

> ```haskell
> Just (('a', 'b'), "")
> ```


This is the kind of constructs we will use to convert the raw parsed
structure into our own types (e.g: an AST).


```haskell
data AST =
    Foo String
  | Bar String
  | Pair Char Char
  deriving (Show)

parseFoo, parseBar, parsePair :: Parser AST
parseFoo = Foo <$> string "foo"
parseBar = Bar <$> string "bar"
parsePair = Pair <$> oneChar <*> oneChar
```


```haskell
runParser parseFoo "foo bar"
runParser parseBar "bar baz"
runParser parsePair "xyz"
```

> ```haskell
> Just (Foo "foo", " bar")
> ```

> ```haskell
> Just(Bar "bar", " baz")
> ```

> ```haskell
> Just (Pair 'x' 'y', "z")
> ```


#### Parser is a Monad

Last but not least, our parser is a [*Monad*](https://en.wikibooks.org/wiki/Haskell/Understanding_monads). Which means it must implement: `>>=`, `>>`, `return` and `fail`:


```haskell
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    -- 1. Run first parser on input.
    -- 2. Feed result of parsing to `f`.
    -- 3. Run second parser (result of `f`) on remaining
    --    input (left by first parser)
    p >>= f = Parser $ \s -> case runParser p s of
                    Nothing -> Nothing
                    Just (a, rest) -> runParser (f a) rest
    -- (>>) :: Parser a -> Parser b -> Parser b
    -- 1. Run first parser on input.
    -- 2. Run second parser on remaining input (left by first parser)
    -- We ignore result of first parser.
    p1 >> p2 = Parser $ \s -> case runParser p1 s of
                    Nothing -> Nothing
                    Just (_, rest) -> runParser p2 rest
    -- return :: a -> Parser a
    return = pure
    -- fail :: String -> Parser a
    fail _ = Parser (const Nothing)
```

Thanks to this definition we can use the `do` syntactic sugar, which
will ease the implementation of more complex parsers. Let's see what we
can do.


```haskell
-- Parse a specific `Char` from the input
char :: Char -> Parser Char
char c = do
    c1 <- oneChar
    if c == c1
       then return c1
       else failure
```


```haskell
runParser (char 'H') "Hello!"
runParser (char 'e') "Hello!"
```


> ```haskell
> Just(‘H’,“ello!”)
> ```

> ```haskell
> Nothing
> ```

We can also implement a cleaner version of our `string` parser (found above):


```haskell
-- Parse a specific pattern from the input
string' :: String -> Parser String
string' [] = return []
string' (c:xs) = do
    c1 <- char c
    rest <- string' xs
    return (c1:rest)
```


```haskell
runParser (string' "Hello") "Hello"
runParser (string' "Hello") "Foo"
```

> ```haskell
> Just("Hello", "")
> ```

> ```haskell
> Nothing
> ```


The `do` notation makes it very easy to combine parsers! We now have
some basic building blocks that we could use to implement more parsing
combinators: `choice`, `many`, `option`, etc. But I'll leave it as an
exercise.

Moreover, it would be interesting to implement an error reporting
mechanism, as well as position tracking (to locate errors in the input),
but I'll leave it for another blog-post (or as an exercise for the
reader!).

## What I learned while reinventing the wheel

Implementing (very) basic parsing combinators led me to better
understand the foundation of libraries like *Parsec* or *Attoparsec*,
and to implement not so trivial instances of typeclasses like
*Applicatives* and *Monads*. Although basic, I think it's a good way
to be more familiar with the *DSL*-like capabilities of Haskell, and
to feel the power that the language offers in term of domain-specific
modeling.
