#!/usr/bin/env python

from pathlib import Path
from random import random
import dbm.gnu as dbm
import sqlite3
import time


MAX_RECORDS = 10000
WRITES = [str(i) for i in range(MAX_RECORDS)]
RANDOM_READS = [str(int(random() * MAX_RECORDS)) for _ in range(MAX_RECORDS)]


class SqliteDict:
    def __init__(self, con):
        self.con = con
        self.con.execute("CREATE TABLE store(key TEXT PRIMARY KEY, value TEXT)")
        self.con.execute("PRAGMA journal_mode = WAL")

    def __setitem__(self, key, value):
        with self.con:
            self.con.execute("INSERT INTO store VALUES (?,?)", (key, value))

    def __getitem__(self, key):
        with self.con:
            return self.con.execute("SELECT value FROM store WHERE key=?", (key,))


def sqlite3_mem_open():
    with sqlite3.connect(":memory:") as con:
        yield SqliteDict(con)


def sqlite3_file_open():
    with sqlite3.connect("file.sql") as con:
        yield SqliteDict(con)

    for file in Path("./").rglob("file.*"):
        file.unlink()


def dbm_open():
    name = "dbm.db"
    with dbm.open("dbm", "c") as db:
        yield db

    for file in Path("./").rglob("dbm"):
        file.unlink()


def dict_open():
    yield {}


def bench(db_gen):
    for db in db_gen():
        t = time.time()

        # create some records
        for i in WRITES:
            db[i] = "x"

        # do a some random reads
        for i in RANDOM_READS:
            x = db[i]

        time_taken = time.time() - t
        print(
            "took %0.3f seconds, %0.5f microseconds / record"
            % (time_taken, (time_taken * 1000000) / MAX_RECORDS)
        )


def bench_sqlite():
    with sqlite3.connect("db.sql") as con:
        with con:
            con.execute("CREATE TABLE store(key TEXT PRIMARY KEY, value TEXT)")
            con.execute("PRAGMA journal_mode = WAL")
        t = time.time()
        with con:
            con.executemany(
                "INSERT INTO store VALUES (?,?)", ((k, "x") for k in WRITES)
            )
        with con:
            con.execute(
                "SELECT * FROM store WHERE key in ({0})".format(
                    ", ".join("?" for _ in RANDOM_READS)
                ),
                RANDOM_READS,
            ).fetchall()
        time_taken = time.time() - t
        print(
            "took %0.3f seconds, %0.5f microseconds / record"
            % (time_taken, (time_taken * 1000000) / MAX_RECORDS)
        )

    for file in Path("./").rglob("db.*"):
        file.unlink()


def bench_sqlite_memory():
    with sqlite3.connect(":memory:") as con:
        with con:
            con.execute("CREATE TABLE store(key TEXT PRIMARY KEY, value TEXT)")
            con.execute("PRAGMA journal_mode = WAL")
        t = time.time()
        with con:
            con.executemany(
                "INSERT INTO store VALUES (?,?)", ((k, "x") for k in WRITES)
            )
        with con:
            con.execute(
                "SELECT * FROM store WHERE key in ({0})".format(
                    ", ".join("?" for _ in RANDOM_READS)
                ),
                RANDOM_READS,
            ).fetchall()
        time_taken = time.time() - t
        print(
            "took %0.3f seconds, %0.5f microseconds / record"
            % (time_taken, (time_taken * 1000000) / MAX_RECORDS)
        )


def bench_sqlite_index_before_insertions():
    with sqlite3.connect("db.sql") as con:
        with con:
            con.execute("CREATE TABLE store(key TEXT PRIMARY KEY, value TEXT)")
            con.execute("PRAGMA journal_mode = WAL")
            con.execute("CREATE INDEX key_index ON store(key)")
        t = time.time()
        with con:
            con.executemany(
                "INSERT INTO store VALUES (?,?)", ((k, "x") for k in WRITES)
            )
        with con:
            con.execute(
                "SELECT * FROM store WHERE key in ({0})".format(
                    ", ".join("?" for _ in RANDOM_READS)
                ),
                RANDOM_READS,
            ).fetchall()
        time_taken = time.time() - t
        print(
            "took %0.3f seconds, %0.5f microseconds / record"
            % (time_taken, (time_taken * 1000000) / MAX_RECORDS)
        )

    for file in Path("./").rglob("db.*"):
        file.unlink()


def bench_sqlite_index_after_insertions():
    with sqlite3.connect("db.sql") as con:
        with con:
            con.execute("CREATE TABLE store(key TEXT PRIMARY KEY, value TEXT)")
            con.execute("PRAGMA journal_mode = WAL")

        t = time.time()
        with con:
            con.executemany(
                "INSERT INTO store VALUES (?,?)", ((k, "x") for k in WRITES)
            )

        with con:
            con.execute("CREATE INDEX key_index ON store(key)")

        with con:
            con.execute(
                "SELECT * FROM store WHERE key in ({0})".format(
                    ", ".join("?" for _ in RANDOM_READS)
                ),
                RANDOM_READS,
            ).fetchall()
        time_taken = time.time() - t
        print(
            "took %0.3f seconds, %0.5f microseconds / record"
            % (time_taken, (time_taken * 1000000) / MAX_RECORDS)
        )

    for file in Path("./").rglob("db.*"):
        file.unlink()


if __name__ == "__main__":
    print("dict – ", end="")
    bench(dict_open)
    print("dbm – ", end="")
    bench(dbm_open)
    print("sqlite3 (executemany + :memory:) – ", end="")
    bench_sqlite_memory()
    print("sqlite3 (executemany + file) – ", end="")
    bench_sqlite()
    print("sqlite3 (executemany + index before insertions) – ", end="")
    bench_sqlite_index_before_insertions()
    print("sqlite3 (executemany + index after insertions) – ", end="")
    bench_sqlite_index_after_insertions()
    print("sqlite3 (:memory:) – ", end="")
    bench(sqlite3_mem_open)
    print("sqlite3 (file)  – ", end="")
    bench(sqlite3_file_open)
