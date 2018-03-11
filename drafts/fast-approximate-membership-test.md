---
title: Fast Approximate Membership Test
date: 2017-11-26
lang: en
logo: v8
---

I recently came up with a small probabilistic data structure to perform the two
following tasks:

* Approximate membership test

> Is string `s` probably in a set of strings?

* Approximate set inclusion test

> Is one set of strings `s1` probably included in another set of strings `s2`?

Moreover, the way the data is represented and stored makes it very
compact and trivial to serialize and de-serialize (they are just
strings).

TODO, find a way to explain the idea visually

str -> tokens
token1 -> hash 32 bits -> packed N chars -> c1
token2 -> hash 32 bits -> packed N chars -> c2
...
tokenN -> hash 32 bits -> packed N chars -> cN

[packed] -> sorted and non repetitive string -> "c1c2...cN"

Checking if a token is in the set:

* token -> hash 32 bits -> packed N chars -> cK
* find index of `cK` in compact string (if any), can be done with binary search
  (`log(N)`)
* find if a set `s1` is included in another set `s2` can be done in
  `O(min(len(s1), len(s2)))`

## Tokenize (Optional)

```javascript
function tokenize(str) {
  return str.match(/\w+/g);
}
```

## Hashing

Although nothing presents us from using a more fancy `hash` function, I decided
to go with the following simple implementation. There is trade-off between
*speed* and collisions here. Using [FNV](https://web.archive.org/web/20131112133856/http://isthe.com/chongo/tech/comp/fnv/)
instead would lower the rate of collisions, but it's `8x` slower! Whether more
collision for better performance is acceptable or not, depends on the use case.
For the remaining of the article, we will assume that this is a reasonable
trade-off.

```javascript
function hash(str) {
  let hash = 5381;
  for (let i = 0; i < str.length; i += 1) {
    hash = (hash * 33) ^ str.charCodeAt(i);
  }
  return (hash >>> 0);
}
```

To get an idea of the speed, I benchmarked it on my laptop (i7 U6600 + 16GB),
using the [benchmark](https://www.npmjs.com/package/benchmark) library. Here
are the results, in number of iterations per second, for strings of size
`4`, `8`, `16`, `32`, `64`, `128`:

> ```sh
> hash 4   x 100,614,253 ops/sec ±0.59% (92 runs sampled)
> hash 8   x 61,245,049 ops/sec ±0.35% (93 runs sampled)
> hash 16  x 32,789,982 ops/sec ±0.65% (93 runs sampled)
> hash 32  x 4,906,633 ops/sec ±0.41% (96 runs sampled)
> hash 64  x 2,717,015 ops/sec ±0.32% (93 runs sampled)
> hash 128 x 1,434,876 ops/sec ±0.36% (95 runs sampled)
> ```


## Packing

Now the trick is to take each hash, and to *pack* it to a valid string
of size `N`. Using `2` unicode characters you can represent numbers from
`0` to `2147483648` (which is the maximum size we allow for the hash).

```javascript
function pack(h) {
  return String.fromCharCode(h & 65535) + String.fromCharCode(h >>> 16);
}
```

The *packed* version of the hash can be *unpacked* using the following function:

```javascript
function unpack(packed) {
  return (packed.charCodeAt(0) | (packed.charCodeAt(1) << 16));
}
```

Again, let's look at some numbers to get a feeling of how fast this should run
in practice:

> ```sh
> pack   x 13,270,921 ops/sec ±0.64% (83 runs sampled)
> unpack x 540,365,673 ops/sec ±0.21% (94 runs sampled)
> ```

To simplify the logic and demonstrate the idea, we will only consider packed
representation of size `1` for the next section. We will discuss a
generalization of the ideas to any size (mainly to decrease the collision rate)
in the last section of this article.

## Compact Set

Now that we have everything we need, we can define what a *compact set* is:

```javascript
function createCompactSet(str) {
    const tokens = tokenize(str);
    const hashes = tokens.map(hash);
    const packed = hashes.map(pack);

    // If no token is provided, don't bother!
    if (packed.length === 0) {
      return '';
    }

    // Sort packed representations of tokens
    packed.sort();

    // Create a sorted string with *no repetition*, of all the
    // packed tokens.
    let result = sorted[0];
    let lastIndex = 0;
    for (let i = 1; i < sorted.length; i += 1) {
      if (sorted[i] !== result[lastIndex]) {
        result += sorted[i];
        lastIndex += 1;
      }
    }

    return result;
}
```

## Membership Test

TODO, visualize
TODO, complexity
TODO, perf

```javascript
function hasEmptyIntersection(s1, s2) {
  let i = 0;
  let j = 0;

  while (i < s1.length && j < s2.length && s1[i] !== s2[j]) {
    if (s1[i] < s2[j]) {
      i += 1;
    } else if (s2[j] < s1[i]) {
      j += 1;
    }
  }

  return !(i < s1.length && j < s2.length);
}
```

## Inclusion Test

TODO, visualize
TODO, complexity
TODO, perf

```javascript
function isIncluded(s1, s2) {
  let lastIndex = 0;
  for (let i = 0; i < s1.length; i += 1) {
    const c = s1[i];
    // Find the occurrence of `c` in `s2`
    const j = s2.indexOf(c, lastIndex);
    if (j === -1) { return false; }
    lastIndex = j + 1;
  }

  return true;
}
```

# Collision Probability

# Generalization

TODO: Generalizing from size `1` to `N` and the impact on memory footprint and
collision rate. (and speed?)

# Comparison with bloom filters

# Conclusion
