
import dbm
import sqlite3
import os

from random import random
import time


MAX_RECORDS = 10000
WRITES = [str(i) for i in range(MAX_RECORDS)]
RANDOM_READS = [
    str(int(random() * MAX_RECORDS))
    for _ in range(MAX_RECORDS)
]

class SqliteDict:
    def __init__(self, con):
        self.con = con
        self.con.execute('CREATE TABLE store(key TEXT, value TEXT)')

    def __setitem__(self, key, value):
        with self.con:
            self.con.execute('INSERT INTO store VALUES (?,?)', (key, value))
    def __getitem__(self, key):
        with self.con:
            return self.con.execute('SELECT value FROM store WHERE key=?', (key,))

def sqlite3_mem_open():
    with sqlite3.connect(':memory:') as con:
        yield SqliteDict(con)

def sqlite3_file_open():
    with sqlite3.connect('file.sql') as con:
        yield SqliteDict(con)
    try:
        os.remove('file.sql')
    except os.FileNoteFoundError:
        print('Could not find', 'file.sql')

def dbm_open():
    name = 'dbm.db'
    with dbm.open('dbm', 'c') as db:
        yield db
    try:
        os.remove(name)
    except os.FileNoteFoundError:
        print('Could not find', name)

def dict_open():
    yield {}


def bench(db_gen):
    for db in db_gen():
        t = time.time()

        # create some records
        for i in WRITES:
            db[i] = 'x'

        # do a some random reads
        for i in RANDOM_READS:
            x = db[i]

        time_taken = time.time() - t
        print("Took %0.3f seconds, %0.5f microseconds / record" % (time_taken, (time_taken * 1000000) / MAX_RECORDS))

def bench_sqlite():
    with sqlite3.connect('db.sql') as con:
        with con:
            con.execute('CREATE TABLE store(key TEXT, value TEXT)')
        t = time.time()
        with con:
            con.executemany('INSERT INTO store VALUES (?,?)', (
                (k, 'x') for k in WRITES
            ))
        with con:
            con.execute('SELECT * FROM store WHERE key in ({0})'.format(', '.join('?' for _ in RANDOM_READS)), RANDOM_READS).fetchall()
        time_taken = time.time() - t
        print("Took %0.3f seconds, %0.5f microseconds / record" % (time_taken, (time_taken * 1000000) / MAX_RECORDS))


if __name__ == "__main__":
    bench_sqlite()
    bench(dict_open)
    bench(dbm_open)
    bench(sqlite3_mem_open)
    bench(sqlite3_file_open)
