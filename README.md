# named-functions

Scala 3 macros for converting multi-parameter functions into functions with named parameters or named-tuple arguments, preserving the original parameter names.

## Installation

Available on Maven Central as `org.polyvariant::named-functions`.

In scala-cli:

```scala
//> using dep org.polyvariant::named-functions::<version>
```

In sbt:

```scala
libraryDependencies += "org.polyvariant" %% "named-functions" % "<version>"
```

In Mill:

```scala
ivy"org.polyvariant::named-functions:<version>"
```

## Usage

```scala
import namedfunctions.syntax.*

def foo(entityId: Int, userId: String): Boolean = ???

// Wrap a function so that its parameters are named at the call site
val f = foo.named
f(entityId = 1, userId = "hello")

// Convert a function into a Function1 from a named tuple
val g = foo.namedTupled
g((entityId = 1, userId = "hello"))
```

### `NamedFunctions.of` / `NamedFunctions.apply` / `.named`

Converts a multi-parameter function `(a: A, b: B, ...) => R` into a function with named parameters, preserving the original parameter names.

### `NamedFunctions.tupled` / `.namedFunctions`

Like `.tupled` but the resulting tuple type carries the parameter names from the original method. Converts a multi-parameter function into a `Function1` from a named tuple: `((a: A, b: B, ...)) => R`.
