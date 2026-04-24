# named-functions

Scala 3 macros for converting multi-parameter functions into functions with named parameters or named-tuple arguments, preserving the original parameter names.

## Quick overview

```scala
import namedfunctions.syntax.*

def greet(name: String, age: Int): String = s"$name is $age"

// namedTuple(...) — build a named tuple from local names
val age = 30
val name = "Alice"
val nt = namedTuple(name, age)

// .named — named parameters at call site
val f = greet.named
f(name = "Alice", age = 30) // "Alice is 30"

// .namedTupled — function from named tuple
val g = greet.namedTupled
g((name = "Alice", age = 30)) // "Alice is 30"

// .namedUntupled — reverse of tupling
val h: ((name: String, age: Int)) => String = t => s"${t.name} is ${t.age}"
h.namedUntupled(name = "Alice", age = 30) // "Alice is 30"

// .nameChecked — compile-time name validation, order-independent
val age = 30
val name = "Alice"
greet.nameChecked(age, name) // "Alice is 30" — reordered automatically
// greet.nameChecked(age, x) // compile error: unexpected: x; missing: name

// .applyProduct — apply from case class fields by name
case class Person(age: Int, name: String, email: String)
greet.applyProduct(Person(30, "Alice", "a@b.com")) // "Alice is 30" — extra fields ignored
```

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

val entityId = 1
val userId = "hello"
val params = namedTuple(entityId, userId)

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

### `syntax.namedTuple`

Builds a named tuple directly from variable references or field accesses by using the final name segment as the label:

```scala
import namedfunctions.syntax.*

val a = 42
val b = "hello"

namedTuple(a, b) // (a = 42, b = "hello")
```

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

## Chaining

Features can be composed — the output of one transformation is a valid input for another:

```scala
def foo(a: Int, b: String): String = s"$a-$b"

case class Params(b: String, a: Int)
foo.named.applyProduct(Params("hello", 42)) // "42-hello"

val a = 42
val b = "hello"
foo.named.nameChecked(a, b) // "42-hello"

// Round-trip through tupling and back
foo.namedTupled.namedUntupled.applyProduct(Params("hello", 42)) // "42-hello"
```

## Cats integration

The cats-based utilities live behind a separate import so they can move to a separate module later:

```scala
import namedfunctions.syntax.*
import namedfunctions.catssyntax.*

def foo(entityId: Int, userId: String): Boolean = ???

((entityId = Option(1), userId = Option("hello"))).namedTupled
// Some((entityId = 1, userId = "hello"))

((entityId = Option(1), userId = Option("hello"))).namedMapN(foo.namedTupled)
// Some(true)

((entityId = Option(1), userId = Option("hello"))).namedParMapN(foo.namedTupled)
```

## Limitations

All features require the macro to extract parameter names from the call site's AST. This works with method references (`obj.method`), eta-expanded methods, and case class constructors (`Foo.apply`), but **not with function values stored in a `val`**:

```scala
val f = (a: Int, b: String) => s"$a-$b"
f.named          // compile error: Could not extract parameter names
f.nameChecked(a, b) // same
f.applyProduct(p)   // same
```

The parameter names exist in the lambda at the definition site, but are erased by the time the `val` reference reaches the macro.
