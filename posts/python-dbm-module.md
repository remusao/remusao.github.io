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
[dbm_bench.py](../snippets/dbm_bench.py)*
