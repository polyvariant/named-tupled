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

// Convert a Function1 from a named tuple back into a named-parameter function
val h: ((entityId: Int, userId: String)) => Boolean = ???
val f2 = h.namedUntupled
f2(entityId = 1, userId = "hello")
```

### Multiple parameter lists

Methods with multiple parameter lists are supported — the result is a curried function with named parameters at each level:

```scala
def bar(entityId: Int)(userId: String): Boolean = ???

val f = bar.named
f(entityId = 1)(userId = "hello")

val g = bar.namedTupled
g((entityId = 1))((userId = "hello"))
```

### `NamedFunctions.of` / `NamedFunctions.apply` / `.named`

Converts a multi-parameter function `(a: A, b: B, ...) => R` into a function with named parameters, preserving the original parameter names.

### `NamedFunctions.tupled` / `.namedTupled`

Like `.tupled` but the resulting tuple type carries the parameter names from the original method. Converts a multi-parameter function into a `Function1` from a named tuple: `((a: A, b: B, ...)) => R`.

### `NamedFunctions.untupled` / `.namedUntupled`

The reverse of `tupled`. Converts a `Function1` from a named tuple into a multi-parameter function with named parameters.

### `.nameChecked`

Compile-time check that argument variable names exactly match the function's parameter names. Arguments are matched by name and automatically reordered, so order doesn't matter — only that the names are correct:

```scala
def foo(a: Int, b: String): String = s"$a-$b"

val a = 42
val b = "hello"

foo.nameChecked(a, b)    // compiles — "42-hello"
foo.nameChecked(b, a)    // also compiles — reordered to "42-hello"
foo.nameChecked(b, x)    // compile error: unexpected: x; missing: a
foo.nameChecked("hello") // compile error: requires variable references or field accesses
```

Arguments can be plain variable references or field accesses (e.g. `obj.field`). The last segment of the access is used as the name. Multiple parameter lists are supported — all arguments are passed flat:

```scala
def bar(entityId: Int)(userId: String): Boolean = ???

val entityId = 1
val userId = "hello"
bar.nameChecked(entityId, userId)
```

Field accesses work too — the field name is what matters:

```scala
case class Source(a: Int, b: String)
val src = Source(42, "hello")
foo.nameChecked(src.a, src.b) // compiles — "42-hello"
```

### `.applyProduct`

Applies a function using fields from a case class, matched by name (not position). The case class may have extra fields, but all function parameters must be present:

```scala
def foo(a: Int, b: String): String = s"$a-$b"

case class Params(b: String, a: Int)
foo.applyProduct(Params("hello", 42)) // "42-hello" — fields matched by name

case class Extended(a: Int, b: String, extra: Boolean)
foo.applyProduct(Extended(1, "hi", true)) // works — extra fields ignored
```

Multiple parameter lists are supported:

```scala
def bar(entityId: Int)(userId: String): Boolean = ???

case class Params(entityId: Int, userId: String)
bar.applyProduct(Params(1, "hello"))
```
