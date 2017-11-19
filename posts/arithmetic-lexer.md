---
title: Arithmetic lexer
date: 2011-12-27
logo: c++
lang: en
---

As part of a project dealing with implementing algorithm to do some
arithmetic on numbers of arbitrary size, arbitrary base and arbitrary
symbols, I had to think about how to write a flexible lexer, capable of
cutting very quickly stream of characters (an arithmetic expression) in
tokens understandable by a parser. So I will write some various articles
on the lexer, parser and evaluation of an arithmetic expression by
making it a point to highlight the various optimizations that I made in
the algorithms to make them as fast as possible. The language used is
C++, but since our priority is to reach the best performances possible,
we will avoid too costly features such as inheritance, virtual class,
etc. .. This article will details the following points:

1. Input and number representation.
2. Memory management.
3. The Lexer.

## Input and number representation

In order not to lose precious seconds in unnecessary operations such as
resizing of string, repeated IO operations, etc… An expression input
will consist of:

1. Length of the base.
2. Symbols of the base.
3. Length of the arithmetic expression.
4. The arithmetic expression, without blank characters..

A correct input could be:

```
1 5 abcde 10 bc+bca+dec
```

So we can easily read the input data with the following code:

```c
char newline;

// Read the base size
unsigned base_size;
std::cin >> base_size;
std::cin.get(newline);

// Read the base's symbols
char* base = new char[base_size + 1];
std::cin.read (base, base_size + 1);
base[base_size] = '\0';

// Read the expression's size
unsigned expr_size;
std::cin >> expr_size;
std::cin.get(newline);

// Read the expression
char* expr = new char[expr_size + 1];
std::cin.read (expr, expr_size + 1);
expr[expr_size] = '\0';
```

The variable named newline will only be used to « absorb » the \n at
the end of each line. Our expression is represented as a simple array
of characters (we could have done the same thing in C). If you want to
compile the code above, do not forget to add a #include <iostream> at
the top of your file.

Since the role of the lexer is to « cut » our expression into
tokens (numbers, operators, parentheses), we must ask ourself in what
form we will store our numbers, which tokens we will use, etc… It
would be tempting to create a string and to copy the number value
there, it would be the most intuitive. But since we are looking
for maximum performances, we will avoid as possible to use dynamic
memory allocation. Instead we will represent our number as a pair of
integers (offset, size) representing the beginning of the number in the
expression (i.e. the index of its first digit in the array representing
the expression) and its length. So we can work directly on the string
containing the arithmetic expression.

Concerning tokens, we can use a simple enumeration containing an entry for each symbol:

```c
typedef struct
{
  unsigned  offset;
  unsigned  size;
} s_number;

typedef enum
{
  PLUS    = 3,
  MINUS   = 4,
  MULT    = 5,
  DIV     = 6,
  MOD     = 7,
  LPAR    = 2,
  RPAR    = 8,
  NUMBER  = 1
} e_token;
```

We will see later why each field of the list is associated with a number.

## Memory management

We will try as much as possible to do it without dynamic memory
allocation. Our couples (offset, size) and our tokens are then stored
on the stack, using the static allocation, avoiding calls to new/delete
and malloc/free. As we are using the string containing the expression
to store our intermediate results, our memory consumption will be
optimized. We must therefore adapt our algorithms for arithmetic in
order to store the results directly in the string representing the
expression.

## The Lexer

We will create a `Lexer class`, containing methods for lexing and some
private variables such as the string containing the expression, the
current offset, etc. ..

The use of an object has several advantages in our case:

1. This prevents parsing functions to take a lot of arguments (with
the object of type `Lexer`, we can afford to give a single argument to
functions).
2. To determine the next token, the lexer needs some informations,
including the expression, the offset, the variable used to return
the possible pair (offset, size) representing a number, etc. .. All
variables can be stored in the object.
3. This makes the code more readable, without reducing the performances
of the program.

Our `Lexer` will be defined as follows:

```c++
class Lexer
{
  public:
    Lexer (char* expr, char* table,
           unsigned expr_size, char* op)
      : num_ (s_number ()),
        expr_ (expr),
        offset_ (0),
        table_ (table),
        expr_size_ (expr_size),
        op_ (op) {}
    ~Lexer () {}

    unsigned get_token ();

    s_number num_;

  private:

    int
    get_op_ (char c)
    {
      return (int)op_[(int)c];
    }

    char* expr_;
    unsigned offset_;
    char* table_;
    unsigned expr_size_;
    char* op_;
};
```

You have probably noticed that the constructor of our object `Lexer` takes
two string parameters named respectively, « table » and « op ». Why
are they useful ?

**Table** - Since our program must be able to handle numbers represented
in an arbitrary base with a set of arbitrary symbols, it would be
very difficult to evaluate expressions without modification to make
them easier to compute. So we’re going (during lexing) to transform
numbers by changing the symbols which they are composed of by a set of
contiguous numbers starting from 0 and up to (base_size – 1). For
example:

```
Old base : abcde
New base : 01234
```

It does not change the size of the base, but just the symbols, by this
wait it becomes easier to perform arithmetic operations on numbers.

**Op** – The purpose of the variable op is different. During lexing,
we want to determine, for each symbol read, if it’s an operator or
not. We can imagine that for expressions of million of characters,
it makes us millions of tests just for the lexing. We will therefore
greatly reduce the number of tests by creating an array of 256
characters which associate, for each operator (using the ASCII code as
an index in the table) the number that corresponds (the one present
in the enumeration given above) and setting the value 0 to all other
characters. This allows us, first to test if a character is an operator,
and at the same time to know the value associated (to determine which
operator it is).

Finally, the public variable `num_` will allow the parser, when the lexer
will return a token of type NUMBER, to get the value of the number that
will be presented in this variable.

We code, we optimize a bit and here is the result:

```c++
unsigned
Lexer::get_token ()
{
  if (offset_ >= expr_size_)
    return 0;

  int tok = get_op_ (expr_[offset_++]);

  if (tok)
    return tok;

  expr_[offset_ - 1] = table_[(int)expr_[offset_ - 1]];

  num_.offset = offset_ - 1;
  unsigned length = 1;

  while (!op_[(int)expr_[offset_]])
  {
    expr_[offset_] = table_[(int)expr_[offset_++]];
    length++;
  }

  num_.size = length;

  return NUMBER;
}
```

This code is not the most optimized but in this version of the program
we will use it like this. Later, during the profiling phase, if it turns
out that this is part of the program is the slowest, we could optimize
it.
