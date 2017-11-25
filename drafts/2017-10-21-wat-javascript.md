---
title: WAT Javascript
---

### Basic Arithmetic

```javascript
> [] + []
''
```

```javascript
> [] + {}
'[object Object]'
```

```javascript
> {} + []
0
```

```javascript
> {} + [42]
42
```

```javascript
> [1] + [2]
'12'
```

```javascript
> null + []
'null'
```

You don't say...
```javascript
> '42' / null
Infinity
```

```javascript
> ('42' / null) - ('42' / null)
Infinity
```

### Int Parsing

```javascript
> parseInt([1, 2, 3 ,4])
1
```

```javascript
> parseInt({})
NaN
```

```javascript
> parseInt('   42foo')
42
```

How deep is deep enough?
```javascript
> parseInt([[[[[[[42]]]]]]])
42
```

### Types Anyone?

```javascript
> typeof null
'object'
> null instanceof Object
false
> null instanceof null
TypeError: Right-hand side of 'instanceof' is not an object
```

```javascript
> '42' | 0
42
```

```javascript
> null | 0
0
```

No matter what you do, Javascript will find a way...
```javascript
> '10'|'0'|[]|null|undefined|'42'|{}
42
```

Concat all the things!!
```javascript
> '42'.concat([], {}, 'foo', 42, null, undefined)
'42[object Object]foo42nullundefined'
```

`replace` only replaces the first occurrence:
```javascript
> 'foo+bar+baz'.replace('+', ' ')
'foo bar+baz'
```

WAT.
