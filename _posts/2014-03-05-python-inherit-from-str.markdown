---
layout: post
title:  "Python inheriting from str class"
date:   2014-03-05 14:50:40
published: false
categories: python
---


```python
class MyString(str):

    def __init__(self, s):
        str.__init__(s)

    def __repr__(self):
        return self

    def __str__(self):
        return self

if __name__ == '__main__':
    s = String("foo")

    # Runtime Error
    print(s)
```
