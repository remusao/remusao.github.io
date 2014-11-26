---
layout: post
title:  "Python inheriting tuple class"
date:   2014-03-05 10:15:40
published: false
categories: python
---

I recently faced some problem while using inheritance in Python.

```python

def foo(arg):
    # Do something
    return tuple(result)

class MyTuple(tuple):

    def __init__(self, arg):
        result = foo(arg)

        # Call constructor from base class tuple
        tuple.__init__(arg)

        # False
        assert self == result

        # True
        assert self == arg
```

So what is the problem here ?
