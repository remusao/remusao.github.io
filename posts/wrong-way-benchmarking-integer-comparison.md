---
title: The wrong way of benchmarking integer comparison functions
date: 2017-11-25
logo: v8
lang: en
---

I recently stumbled upon [an article](https://blogs.msdn.microsoft.com/oldnewthing/20171117-00/?p=97416),
describing how trying to out-smart the compiler can result in bad
performances compared to more naive code. The benchmark is about finding
the most efficient way to write a comparison function between integers,
to be used in conjunction with `sort` or binary search for example. The
original post being about `c++`, I was curious to know if the results
would transfer to JavaScript (in particular `V8`, using `Node.js`). This
is what this post is about.

Let see the contenders:


* *Naive*

```javascript
function compare1(a, b) {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
}
```

* *Clever*

```javascript
function compare2(a, b) {
    return a - b;
}
```

## Benchmarks

The original article has a very good point saying that to benchmark this
code realistically, you need a real use-case. Just sorting the array and
summing the numbers does not make any sense, no one would do it. But if
instead you perform a binary search on the sorted array, it gets closer
to something one might do in a real code-base.

I took the liberty of converting the original `C++` implementation of the binary
search into JavaScript, to stick closely to the article:

```javascript
function binarySearch(array, first, last, key, compare) {
  var length = last - first;
  while (length > 0) {
    var step = (length / 2) | 0;
    var middle = first + step;
    var result = compare(array[middle], key);
    if (result < 0) {
      first = middle + 1;
      length -= step + 1;
    } else if (result === 0) {
      return middle;
    } else {
      length = step;
    }
  }

  return last;
}
```

Using the great [benchmark](https://www.npmjs.com/package/benchmark)
library, let's evaluate the performance of each compare function:

* *Setup*

```javascript
function run(compare) {
  // Create a sorted array
  var array = [];
  for (var i = 0; i < 8192; ++i) {
    array.push(i * 2);
  }

  var found = 0;

  for (var j = -1; j < 16383; ++j) {
    var index = binarySearch(
      array,              // sorted array
      0,                  // start index
      array.length - 1,   // end index
      j,                  // element we are looking for
      compare);           // comparison function

    if (index !== -1 && array[index] === j) {
      found += 1;
    }
  }

  if (found !== 8191) {
    console.error('Found bug', found);
  }
}
```

* *Benchmark*

```javascript
function bench() {
  var Benchmark = require('benchmark');
  var suite = new Benchmark.Suite();

  suite
    .add('compare1', function() {
      run(compare1);
    })
    .add('compare2', function() {
      run(compare2);
    })
    .on('cycle', function(event) {
      console.log(String(event.target));
    })
    .on('complete', function() {
      console.log('Fastest is ' + this.filter('fastest').map('name'));
    })
    .run({});
}

bench();
```

And...

> ```sh
> compare1 x 421 ops/sec ±0.51% (91 runs sampled)
> compare2 x 394 ops/sec ±0.39% (89 runs sampled)
> ```

It happens that the naive `compare1` is a bit faster than `compare2`.
It's not day and night, but the *naive*, more readable code is more
efficient on V8 than the clever one.
