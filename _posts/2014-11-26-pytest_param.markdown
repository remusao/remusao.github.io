---
layout: post
title:  "[py.test] Paramaterize tests with external data"
date:   2014-11-26 15:00:00
categories: python pytest
comments: true
---

I recently began to make heavy use of [py.test](http://pytest.org/latest/)
in my day-to-day Python development. It's a wonderful tool, but I won't
explain to you every features it provides and why it's awesome. Instead,
I'll explain how I managed to cleanly externalize the data used for my
tests in external files (that can be of any format: yaml, json, python
files). The idea here is to *separate the code that performs the test*,
from the *input data used to perform the test*.

`test_feature.py`
{% highlight python %}
def test_my_feature(one_example):
    assert one_example
{% endhighlight %}

`data_feature.yaml`
{% highlight yaml %}
tests:
    test1:
        ...
    test2:
        ...
{% endhighlight %}

## First solution: yield

The first solution would be to use `yield` to generate tests, as it's supported
by `py.test` (as long as you don't want to use fixtures in your test function...).

{% highlight python %}
def check(example):
    # perform your test

def test_feature():
    for test in generate_tests():
        yield check, test
{% endhighlight %}

Here, `py.test` will understand that `test_feature` will yield several tests and that
the `check` function must be used to perform the test, so it is almost equivalent to do:

{% highlight python %}
def test_feature():
    for test in generate_tests():
        check(test)
{% endhighlight %}

Except that in this last snippet of code, the tests will stop as soon as one fails.
With the `yield`-version, every tests will be ran even if some fail. This is useful
if you have lot of tests, and you want to know which ones fail (not just the first one).

__Problem__: if you want to use fixtures with your `test_feature` functions, it breaks:

{% highlight python %}
def check(example):
    # perform your test

def test_feature(my_fixture):
    for test in generate_tests():
        yield check, test
{% endhighlight %}

This will tell you that `test_feature` expects one argument but that none are provided.

End of the story...
Wait, no, `py.test` is awesome remember? So there must be a solution!

## Parametrization

One of the cool features of `py.test` is the ability to add parameters on our
tests or fixtures, so that a test is ran once for each parameter (from [py.test doc](http://pytest.org/latest/parametrize.html)):

{% highlight python %}
# content of test_expectation.py
import pytest
@pytest.mark.parametrize("input,expected", [
    ("3+5", 8),
    ("2+4", 6),
    ("6*9", 42),
])
def test_eval(input, expected):
    assert eval(input) == expected
{% endhighlight %}

{% highlight python %}
$ py.test
=========================== test session starts ============================
platform linux -- Python 3.4.0 -- py-1.4.26 -- pytest-2.6.4
collected 3 items

test_expectation.py ..F

================================= FAILURES =================================
____________________________ test_eval[6*9-42] _____________________________

input = '6*9', expected = 42

    @pytest.mark.parametrize("input,expected", [
        ("3+5", 8),
        ("2+4", 6),
        ("6*9", 42),
    ])
    def test_eval(input, expected):
>       assert eval(input) == expected
E       assert 54 == 42
E        +  where 54 = eval('6*9')

test_expectation.py:8: AssertionError
==================== 1 failed, 2 passed in 0.01 seconds ====================
{% endhighlight %}

So here our `test_eval` function has been called *three times*. Once for each parameter.
Great! But what if you want your parameters to come from another file, or from a function.
In other words, what if you want to *dynamically parametrize* your function?


## Hooks at the rescue

[Hooks](http://pytest.org/latest/plugins.html#well-specified-hooks) allow you to plug code into `py.test` at diffent stages of the test run.
The hook that can be useful for us is `pytest_generate_tests` that will allow
to generate several calls to the same test function, but with different arguments
(from [py.test doc](http://pytest.org/latest/funcargs.html#basic-generated-test-example)):

{% highlight python %}
# content of test_example.py
def pytest_generate_tests(metafunc):
    if "numiter" in metafunc.funcargnames:
        metafunc.parametrize("numiter", range(10))

def test_func(numiter):
    assert numiter < 9
{% endhighlight %}

{% highlight python %}
$ py.test test_example.py
=========================== test session starts ============================
platform linux2 -- Python 2.7.1 -- pytest-2.2.4
collecting ... collected 10 items

test_example.py .........F

================================= FAILURES =================================
_______________________________ test_func[9] _______________________________

numiter = 9

    def test_func(numiter):
>       assert numiter < 9
E       assert 9 < 9

test_example.py:6: AssertionError
==================== 1 failed, 9 passed in 0.02 seconds ====================
{% endhighlight %}

Great, so the last things to do is:

1. Detect functions that make use of a fixture whose name starts with `data_`
2. Load the corresponding file or resource for the test source
3. Parametrize the function with each of the data

For example, here is what you can do:

{% highlight python %}
def pytest_generate_tests(metafunc):
    """ This allows us to load tests from external files by
    parametrizing tests with each test case found in a data_X
    file """
    for fixture in metafunc.fixturenames:
        if fixture.startswith('data_'):
            # Load associated test data
            tests = load_tests(fixture)
            metafunc.parametrize(fixture, tests)
{% endhighlight %}

Here, the `load_tests` function takes as argument the name of the fixture `data_X`
and will:

1. Load the corresponding file
2. Extract the different test-cases
3. Return a list of all the cases

For example, if your tests are stored in a Python file:

{% highlight python %}
import importlib


def load_tests(name):
    # Load module which contains test data
    tests_module = importlib.import_module(name)
    # Tests are to be found in the variable `tests` of the module
    for test in tests_module.tests.iteritems():
        yield test
{% endhighlight %}

The data file (`data_my_feature.py`) could look something like:

{% highlight python %}
tests = [
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9
]
{% endhighlight %}

The test function will then be invoked for each case.

{% highlight python %}
def test_feature(data_my_feature):
    assert data_my_feature < 5
{% endhighlight %}

## Conclusion

Here it's not really interesting, but the benefits are numerous:

1. storing your data in a database, or in yaml/json formatted files, or whatever
2. other people can add tests to your project, without having to dig into the code
3. provide a common format to define tests in external files
4. reuse the same data for several tests
5. the data is not hard-coded in Python source-code


TL;DR: `py.test` is awesome. Make tests. Get data for your tests from external sources.
