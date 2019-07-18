---
title: The Sieve, Even faster!
date: 2011-12-27
logo: cpp
lang: en
---

**Disclaimer**: This is an old article and information may be out-dated. You can
find one of the (if not *the*) fastest implementation there: <http://primesieve.org/>

It has been a while since I posted my last article on the blog, my
studies didn’t leave me enough time for writing. But during Christmas
holidays I found some time to share with you some discoveries and
developments that I made recently on the Sieve of Eratosthenes.
This article follows the previous article on the Sieve and aim to
present some optimizations and an attempt to determine the complexity of
the algorithm.

## Last optimizations

Optimizations that follow are the result of several comments on the
above implementations:

* Could we not reduce the memory footprint of the program by working
only on arrays of bits instead of chars ? (We could divide the memory
used by 8).
* In the main loop, our variable i ranges from 1 in 1, and values
are not always prime numbers, so there is no need to eliminate their
multiples.

For the second optimization, we just have to get the next number that
has not yet been eliminated. With a simple loop. We then gain precious
seconds at runtime.

Regarding the first optimization, it turns out that it’s not easy
to work directly on the bits in C, except with bit fields, but this
is complicated in the case of the Sieve of Eratosthenes. So I decided
to implement the algorithm in C++, which offers the type bool, which
takes only one bit in memory as well as Bitset, which are nothing more
than arrays of bits, provided with various operations. Thanks to a
std::vector (or a simple array) of bool the memory usage of the program
is reduced by 8, which is pretty good. But it’s possible to use a data
structure more efficient. You certainly heard about Boost, a library
which provide many very powerful tools for C++, and among this tools
is the Bitset (it also exists in STD, but it is less efficient). Using
Bitset, we gain a little in runtime.

Note that Boost Bitset are initialized to 0 by default, so we must
consider that a number is prime if the value associated in the array is
0 (and not 1 as in a previous implementations).

```c++
#include <cmath>
#include <iostream>
#include <boost/dynamic_bitset.hpp>

void
print_tab (boost::dynamic_bitset<>& tab)
{
  std::cout << 2 << std::endl;
  for (unsigned i = 1; i < tab.size (); i++)
    if (!tab[i])
      std::cout << 2 * i + 1 << std::endl;
}

void
erato (boost::dynamic_bitset<>& tab)
{
  unsigned i = 0;
  unsigned j, step, borne = sqrt (tab.size ());

  for (i = 1; i < borne; i++)
  {
    step = 2 * i + 1;
    for (j = i + step; j < tab.size (); j += step)
      tab[j] = 1;

    while (tab[i])
      i++;
  }
}

int
main (int argc, char **argv)
{
  if (argc != 2)
  {
    std::cout << "Please, you must give a number as argument." << std::endl;
    return 1;
  }

  unsigned size = 0;
  for (unsigned i = 0; argv[1][i]; i++)
    size = size * 10 + (argv[1][i] - '0');

  boost::dynamic_bitset<>* tab = new boost::dynamic_bitset<> (size / 2);

  erato (*tab);
  //print_tab (*tab);
}
```

With this algorithm we get much better performance than with the
previous implementation.

## Complexity approximation

The complexity of an algorithm is used to express the number of
operations needed to execution depending on the size of the input (in
our case it will be the limit up to which we want to calculate the prime
numbers).

Soon I will post a more accurate approximation of the complexity of this
algorithm, but my initial investigations suggest that the complexity is
linear (3xN?).

## The last words

After presenting a number of optimizations to the algorithm of the
Sieve of Eratosthenes, we saw that it is always possible to improve
a program, and this, in two lines of research. First, one can often
optimize the algorithm itself, by choosing appropriate data structures,
trying to reduce the memory footprint or by reducing the number of
calculations needed to compute the result, etc. .. One can also optimize
the implementation of the algorithm, it’s often the last step, during
which we will try to find tips, related to language itself, that allow
us to make our program faster by changing the compiler flags (-O,
-march, etc. ..), etc. ..

The last implementation presented in this article is far from being the
most optimized, we could still find ways to improve performances, for
exemple by eliminating multiples of 3, 5 and 7 at the initialization
of the array or by using bit masks to eliminate several multiples at a
time. However, keep in mind that it is difficult to keep the compactness
and readability of our algorithm with such optimizations.

If you know other optimizations, or have any comments on this article,
feel free to contact me or leave a comment below.
