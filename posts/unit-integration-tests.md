---
title: Thoughts About Unit and Integration Tests
date: 2019-10-26
logo: chai
lang: en
issue: 44
---

> Do not feed the anti-patterns!

I recently found myself working with tests---integration and unit---. This
experience led to some frustration, which got me thinking... Where did the
frustration come from? How could it be done differently? And ultimately, what
makes a good test? This post is a collection of thoughts about testing, not an
exhaustive guide.

# Unit vs. Integration?

> It does not matter which one you pick first, you might need both eventually...

Before writing any test, you might be wondering *which kind of test* makes
sense. Would it be better to cover your new feature with unit or integration
tests? You might want to pause for a moment and think about the trade-offs of
the different options.

Often, *unit tests* will run in a more controlled environment, maybe with some
dependencies [mocked](https://en.wikipedia.org/wiki/Mock_object), so that you
can focus on a specific file, class or function. This means that unit tests are
more targeted than integration tests. It also means that they might be easier
to reason about, and quicker to iterate with. As an example, imagine testing a
JavaScript library; unit tests could be running in Node.js instead of a
full-blown browser.

On the other hand, *integration tests* will potentially target multiple systems
interacting with each other, running in an environment which is much closer to
production conditions. Using the same example, your JavaScript library might now be tested
in a handful of browsers (e.g.: Firefox, Chromium and Safari, with different
versions of each), while interacting with other libraries. This means that you
will most likely gain more confidence that the code works in production
environment, at the cost of a more complex setup.

# Non-regression Tests

> Non-regression is not the same as *non-evolution*.

It is desirable to consolidate APIs or components with non-regression tests,
but be careful not to prevent future evolution! This might happen if you take
the current production system and generate test-cases by feeding inputs,
getting the results and writing high-level tests which ensure that the output
is always the one that was expected *at the time test-cases were generated*.

Although this might feel good at first---we have great test coverage after
all!---, this approach has at least one major short-coming: *it's very hard to
make the tests evolve in the future*. It's nice to "freeze" the capabilities of
your system with numerous tests, but unless said system is not meant to evolve
in the future, the assumptions about expected results will most likely evolve
over time. This means that when writing tests you probably need to think not
only about coverage and the expected behavior *right now*, but also how easy it
will be to make the tests evolve in the future.

Tests are tightly related to the code being tested and, if you are not careful,
rigid tests might make changing the source code much more painful than it
should be.

# Your Tests are Code

> Tests are not the destination, they are the starting point and the path.

There is a misunderstanding at the root of all evils: you shall not *treat
your tests differently than your source code*. Tests **are** code! To make
things a bit more concrete, here is a list of assumptions which I think are
reasonable when it comes to source code but are rarely accepted for tests:

* You expect to be able to *build* and *run* your code easily.
* You expect your code to be *efficient*.
* You expect your code to be *understandable*.
* You expect your code to be *documented*.
* You expect your code to be *cross-platform*.
* You expect your code to be *easy to delete*!
* You expect your code to *not be redundant and use abstractions*.

Whenever one of the expectations holds for your code, try replacing `code` with
`tests` and ask yourself if it is still true. Your code and the tests you write
to ensure it behaves in a correct way will have to evolve side-by-side in the
future. You do not want your tests to become a *liability*.

# Future-Proof Tests?

> No developer ever reads the same code twice. --- Heraclitus

When writing tests, try to express your intent and give cues to the future
reader ---that might be you in a few months!--- regarding why a given assertion was
needed, what behavior is being tested, etc. Alongside the usual features of a
language such as *comments*, *good names* for variables and functions, etc.
modern testing frameworks give many tools to ease this process: use things like
`describe(...)`, `it(...)` and explicit assertions `expect(...).to.be`.

Additionally, as a contributor to a project which has tests, it should be
reasonnably straightforward to answer the following questions:

* when looking at code, where can I find the existing tests?
* when looking at tests, which code is being tested?

There should be a clear path to modify both code and tests:

* when changing code, which tests need to be changed?
* when deleting code, which tests need to be deleted?

A few tips can ease this process. *Naming conventions* for test files should be
consistent and finding the tests for a given file or component should not
involve any interpretation, it should be mechanical (e.g.: tests for `module.js`
are always to be found in `tests/module.test.js`).

Tests should be organized in a way that follows the code structure closely so
that changes in the source can be reflected to the tests and *vice versa*. This
also applies to the internal structure of the tests. It can be helpful to group
assertions by behavior or assumptions. It is also helpful to be explicit about
both success and failure of an assertion!

This last point is important because if you can quickly grasp the intent while
reading a test; it allows to decide if an assertion still holds after a change
in the code, or if it should go away completely.

Last but not least, avoid repetition in tests. If you see yourself writing the
same assertions over and over again to tests similar components, it might be
worth abstracting these away. You will thank yourself in the future when you
need to amend the tests!

# By Way of Conclusion

> Always pick the right hammer for your nails...

As mentioned previously, this is not about picking unit tests over integration
tests. You will likely need both eventually because they are often
complementary and give you a different kind of confidence in the correctness of
your code (although some [argue against integration tests](https://blog.thecodewhisperer.com/permalink/integrated-tests-are-a-scam)).

As a take-away, treat your tests the same way you treat your source code and
hold them both to high standards. Not doing so might lead to a situation where
your tests become a liability and slow you down instead of empowering you to
write better and more robust features.
