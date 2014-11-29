## What is λΠ?

It seems a bit silly to write 4 different type checkers and not
actually specify what language we're dealing with.

In many ways, λΠ is the simplest possible dependently typed
language. It includes what we'd expect from lambda calculus,
functions, applications, and variables. Additionally, I've included
booleans so we also have true and false. Since that didn't feel like
enough, our calculus supports arbitrary constants, not just booleans.

Now in order to actually have some dependent types we add "pi
types". This sounds a lot scarier than it is. Pi types are the
dependently typed equivalent of Haskell's `→`. They let the return
*type* of a function depend on the *value* of the argument.

My new favorite toy instance is to suppose we have lists indexed by
their length. Then `replicate` could be given the type

``` haskell
    replicate :: (n : Int) -> a -> List n a
```

Which should be read as

> If you give me some integer `n` and some value `a`, I'll give you
> back a list with `n` elements.

That's it! That's all that's in lambda pi.
