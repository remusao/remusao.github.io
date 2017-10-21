---
title: How To Get The Most Out Of Sqlite3
---

I've recently made heavy use of `sqlite3` for a project involving a lot of data
and processing. My first attempt involved no database at all, and all data would
be kept in memory and queries would consist in a mix of dictionary lookups,
iteration, conditions, etc. This was nice, but there is only so much you can fit
in memory, and re-generating/loading the data from disk to memory was a tedious
and time consuming process.

I decided to give `sqlite3` a try. This allowed an increase in the amount of
data that could be processed, and reduced the loading time of the application to
nothing, since only opening a connection to the database was needed. Moreover,
I could replace a lot of Python logic by SQL queries.

I'd like to share a few learnings and findings about this experience.


## Use Bulk Operations

If you need to insert a lot of rows at once in your database, you really should
not use `execute`. The `sqlite3` provides a way to bulk insertions: `executemany`.

Instead of doing something like:
```python
for row in iter_data():
    connection.execute('INSERT INTO my_table VALUES (?)', row)
```

You can leverage the fact the `executemany` accepts as argument a generator of
`tuples`:

```python
connection.executemany(
    'INSERT INTO my_table VALUE (?)',
    iter_data()
)
```

This is not only nicer, it's also much more efficient. In fact, `sqlite3`
implements `execute` using `executemany` behind the scene, only it only inserts
one row instead of many.

I wrote a small benchmark which consists in inserting a million rows into an
empty table (the database is lives only in memory):

* `executemany`: **1.6** seconds
* `execute`: 2.7 seconds


## You Don't Need Cursors

...*most of the time*.

One thing I often found confusing at the beginning, was `cursor` management.
Examples online and in the documentation often look like:

```python
cursor = connection.cursor()
# Do something with cursor
```

But most of the time you don't need a cursor at all, and you can directly use
the `connection` object (it is mentioned at the
[end of the documentation](https://docs.python.org/3.6/library/sqlite3.html#using-shortcut-methods)).

Operations such as `execute` and `executemany` can be called directly on the
connection. Here is an example from the official documentation:

```python
import sqlite3

connection = sqlite3(':memory:')

# Create a table
connection.execute('CREATE TABLE events(ts, msg)')

# Insert values
connection.executemany(
    'INSERT INTO events VALUES (?,?)',
    [
        (1, 'foo'),
        (2, 'bar'),
        (3, 'baz')
    ]
)

# Print inserted rows
for row in connnection.execute('SELECT * FROM events'):
    print(row)
```


## Cursors Can Be Iterated Upon

You might often see examples making use of `fetchone` or `fetchall` on
the result of a `SELECT` query. But I find that the most natural way to
consume the results is to actually iterate on the cursor directly:

```python
for row in connection.execute('SELECT * FROM events'):
    print(row)
```

This way, you can stop as soon as you got enough results and not waste
resources. Of course, if you know beforehand how many results you want, you can
use the `LIMIT` SQL statement instead, but Python generator are very handy and
allow you to nicely decouple data generation from data consumption.


## Use Context Managers

Shit happens, even in the middle of a SQL transaction. To avoid having
to deal manually with `rollback` or `commit`, you can simply use the
`connection` object as a context manager. In the following example we
create a table, and insert *by mistake* a duplicated values:

```python
import sqlite3
connection = sqlite3.connect(':memory:')

with connection:
    connection.execute(
        'CREATE TABLE events(ts, msg, PRIMARY KEY(ts, msg))')

try:
    with connection:
        connection.executemany('INSERT INTO events VALUES (?, ?)', [
            (1, 'foo'),
            (2, 'bar'),
            (3, 'baz'),
            (1, 'foo'),
        ])
except (sqlite3.OperationalError, sqlite3.IntegrityError) as e:
    print('Could not complete operation:', e)
    
# No row was inserted because transaction failed
for row in connection.execute('SELECT * FROM events'):
    print(row)
    
connection.close()
```

## Use Pragmas
...*when it makes sense*

There are a few *pragmas* you can use to tweak the behavior of `sqlite3` in your
program. In particular, one that could improve the performance was `synchronous`:

```python
connection.execute("PRAGMA synchronous = OFF")
```

You should be aware though that this can be dangerous. If the application
crashes unexpectedly in the middle of a transaction, the database will probably
be left in an inconsistent state. So use with care! But if you want to insert a
lot of rows faster, that can be an option.

## Postpone Index Creation

Let's say you need a few indices on your database, and you also need to
insert a lot of rows while creating it. Postponing the creation of the indices
to after all rows have been inserted could result in a substantial performance
improvement.


## Use Placeholders to Interpolate Python Values

It is tempting to use Python string operations to include values into
queries. *Do not*! This is highly insecure, and `sqlite3` gives you a
nicer way to do it:

```python
# Do not do this!
my_timestamp = 1
c.execute("SELECT * FROM events WHERE ts = '%s'" % my_timestamp)

# Do this instead
my_timestamp = (1,)
c.execute('SELECT * FROM stocks WHERE symbol=?', my_timestamp)
```

Also, string interpolation using Python `%s` (or format, or formatted string
literals) does not go well with `executemany`. So there is really no point in
trying!


---

Keep in mind though that these tips might or might not give you a benefit,
depending on your specific use-case. You should always try for yourself and
decide if it's worth it or not.

