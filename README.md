# Incremental Untyped Lambda Calculus

Incremental untyped lambda calculus.

This is a first attempt to implement an incremental lambda-calculus based on the [paper and project by Paolo G. Giarrusso](http://inc-lc.github.io/). It's not very useful yet, it's just a toy. This was also based on Stephen Diehl's lambda calculus implementation from ["Write You a Haskell"](http://dev.stephendiehl.com/fun/).

There are many theoretical problems with this implementation. For example, we're not using any canonical name-capturing scheme (like de Bruijn Indexes), so automatic renaming conflicts are likely.

There are many handy built-in functions that map directly to lambda terms: `"true", "false", "and", "or", "not", "ifthenelse", "iszero", "pair", "first", "second", "nil", "succ", "pred", "plus", "mul", "pow", "sub"`. Significantly, there is also a `derive` function built-in that will compute the derivative of a given term.

To compile and run:

```shell
$ cabal run
```

Example
-------

```bash
Untyped> (\x. x) 1
 => \x . x
 => 1
 => x
1

Untyped> derive (\x. x)
(\x dx . dx)
```

License
=======

Released under MIT license.
