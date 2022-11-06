---
title: TIL—Python has a built-in persistent key-value store
date: 2018-06-10
logo: python
issue: 15
lang: en
---

Python is, in a lot of ways, a very rich language. After years of using
it, I still regularly discover new parts of the ecosystem, even in the
standard library. In particular, there are a few modules which are not
very well-known, but can be very useful in some situations. Today I
discovered `dbm` a persistent key/value store:

Quick start:
```python
import dbm

with dbm.open('my_store', 'c') as db:
  db['key'] = 'value'
  print(db.keys()) # ['key']
  print(db['key']) # 'value'
  print('key' in db) # True
```

It behaves a lot like a `dict` except:

* It persists its values on disk
* You can only use `str` or `bytes` as key and values

The performance is also slower than a dictionary, but faster than `sqlite3`.

The benchmark consists in performing *10k* writes and *10k* random reads:

```python
from random import random
import time

operations = 10000
writes = [str(i) for i in range(operations)]
reads = [str(int(random() * operations)) for _ in range(operations)]

# Create some records
for i in writes:
    db[i] = 'x'

# Read values in random order
for i in reads:
    x = db[i]
```

Here are the results:
* `dict` -- total: *0.002 seconds*, 0.23398 μs/record **x 1**
* `dbm` -- total: 0.054 seconds, 5.35984 μs/record **x 27**
* `sqlite3` in-memory: total: 2.468 seconds, 246.84346 μs/record **x 1234**
* `sqlite3` file: total 42.407 seconds, 4240.69593 μs/record **x 21207**

Why is `sqlite3` so slow? Well, the benchmark is probably not representative of
the typical workload for sqlite (lots of individual insertions and selections).
If we perform the same operations using `executemany` and one `select` of all
the keys at once, we get:

* `:memory:` -- total: 0.038 seconds, 3.81415 μs/record **x 19**
* `file` -- total 0.071 seconds, 7.07369 μs/record **x 35**

It's much better, but still not as fast as `dbm` (when we persist to
a file). So, if you have a workload where keys and values are added
and retrieved very often, are always `str` or `bytes` and need to be
persisted on disk, `dbm` is a serious contender!

*The code used to benchmark can be found here:
[dbm_bench.py](https://github.com/remusao/remusao.github.io/blob/d9b5a3088e4cae8452eabf90371f582c81569d88/snippets/dbm_bench.py)*

---

**Edit 06-11-2022**: There was some great feedback on [Hacker News](https://news.ycombinator.com/item?id=32849592) yesterday. I'd like to address some of it.

* *Using a primary key*---It was suggested that adding a `PRIMARY KEY` when
  creating the table improves performance of SQLite3, so I've updated the
  benchmark accordingly. It does help with performance indeed (see below).
* *Using WAL pragma*---It was suggested that using WAL journaling instead of
  default mode would also make more sense. I have updated the benchmarking
  script accordingly.
* *Using one INSERT at a time vs. `executemany`*---It was called out that
  executing one INSERT command per key instead of batching the operations
  is less efficient. The benchmark measures both scenarios: one where single
  INSERTs are made ("sqlite3 (:memory:)" and "sqlite3 (file)") and one where
  `executemany` is used to insert and read everything in one batch ("sqlite3
  (executemany)"). I've added one more benchmark for the use of `executemany`
  on a `:memory` SQLite3 database. The bottomline is that in all these cases,
  SQLite3 is still slower than DBM.
* *Using an index with SQLite3*---In order to improve performances of SQLite3
  operations, creating an index can also be benefitial, and this was suggested
  in the comments. I've added two new entries to the benchmark to show the
  performance when creating an index before insertions and also after
  insertions but before reading the values back.
* *Which DBM implementation is used*---This is something that the initial
  blogpost missed. The results are about the GNU implementation of DBM, which
  the new benchmarking code now explicitely imports. The performance might vary
  vastely between implementations (worst case being if the [dbm.dumb](https://docs.python.org/3/library/dbm.html#module-dbm.dumb) module is used).

With all the above, I have updated the benchmarks and here are the new results obtained with CPython 3.11.0, best of 5 runs:

* `dict`---took 0.001 seconds, 0.11792 microseconds / record
* `dbm`---took 0.009 seconds, 0.88954 microseconds / record
* `sqlite3, executemany, :memory:`---took 0.029 seconds, 2.86062 microseconds / record
* `sqlite3, executemany, file`---took 0.045 seconds, 4.52690 microseconds / record
* `sqlite3, executemany, index after insertions`---took 0.044 seconds, 4.37975 microseconds / record
* `sqlite3, executemany, index before insertions`---took 0.048 seconds, 4.84734 microseconds / record
* `sqlite3, :memory:`---took 0.082 seconds, 8.19964 microseconds / record
* `sqlite3, file` ---took 4.661 seconds, 466.07621 microseconds / record

With all optimizations brought to the SQLite3 version, `dbm.gnu` still has an
edge on this very *synthetic benchmark*. And that is, I think, the bottom line
of this article. It is only that, a synthetic benchmark and you should probably
not read too much into it, unless  your particular use-case involves writing
10,000 random values then reading them back and nothing else. As everything,
you should first perform your own measurements based on your own use-case and
constraints, then make a decision about what technology best serves your need.
