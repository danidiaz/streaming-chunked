# streaming-chunked
Working with streams of packed data.

## What's this for?

Sometimes we want to work with sequences of bytes or chars without keeping them
wholly in memory at any point. Streaming libraries like
[streaming](http://hackage.haskell.org/package/streaming) (of course),
[streamly](http://hackage.haskell.org/package/streamly),
[conduit](http://hackage.haskell.org/package/conduit) and
[pipes](http://hackage.haskell.org/package/pipes) help with that.

However, turns out that yielding individual bytes or chars downstream is not
very efficient. Instead, it's better to yield whole chunks of packed data,
inside which the bytes or chars are contiguous in memory, with less
indirection.

For many functions, we might still want to refer to the *individual*
items. The typical example is `length`: we usually don't want to count the
number of yielded chunks, but the number of bytes or chars! Similarly, we
usually want to split a stream at the nth byte or char, not at the nth
chunk.     

This library builds on
[streaming](http://hackage.haskell.org/package/streaming) and allows working
with packed datatypes in a more natural way.

## Comparison with conduit

[conduit](http://hackage.haskell.org/package/conduit) has `-E` suffixed versions of functions (like [takeWhileE](http://hackage.haskell.org/package/conduit-1.3.2/docs/Data-Conduit-Combinators.html#v:takeWhileE)) that let you
work with streams packed data. What counts as "packed data" is defined by the
[`IsSequence`](http://hackage.haskell.org/package/mono-traversable-1.0.15.1/docs/Data-Sequences.html#t:IsSequence) typeclass. 

Instead of a typeclass , this library uses a [module
signature](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#module-signatures)
to define what counts as "packed data".

## Comparison with streaming-bytestring

This library is similar in philosophy to
[streaming-bytestring](http://hackage.haskell.org/package/streaming-bytestring)
with some differences:

- It aims to be more general, using a [module
  signature](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#module-signatures)
  to allow clients to configure the packed representation they want, instead of
  harcoding it to [bytestring](http://hackage.haskell.org/package/bytestring).

- Fewer dependencies: it doesn't depend on
  [exceptions](http://hackage.haskell.org/package/exceptions) or
  [resourcet](http://hackage.haskell.org/package/resourcet). The main library
  doesn't depend on [bytestring](http://hackage.haskell.org/package/bytestring)
  or [text](http://hackage.haskell.org/package/text), either.

- It doesn't have (at the moment at least) the focus on performance that
  [streaming-bytestring](http://hackage.haskell.org/package/streaming-bytestring)
  has. In particular, in the internals of the library lives a newtyped
  [`Streaming.Stream`](http://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming.html#t:Stream),
  instead of a more specialized representation. We only wrap it for
  expressivity.


