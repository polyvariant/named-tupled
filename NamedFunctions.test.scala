//> using dep org.scalameta::munit::1.3.0

package namedfunctions

import cats.data.NonEmptyChain
import cats.data.EitherNec
import cats.syntax.all.*
import syntax.*

class NamedFunctionsTest extends munit.FunSuite {

  def foo(entityId: Int, userId: String): Boolean = true

  test("of - named-parameter function") {
    val f = foo.named
    val result = f(entityId = 1, userId = "hello")
    assert(result)
  }

  test("tupled - Function1 from named tuple") {
    val g = foo.namedTupled
    val result = g((entityId = 1, userId = "hello"))
    assert(result)
  }

  test("namedTuple composes with namedTupled") {
    val entityId = 1
    val userId = "hello"
    val result = foo.namedTupled(namedTuple(entityId, userId))
    assert(result)
  }

  test("namedTuple composes with namedTupled using field selects") {
    case class Params(entityId: Int, userId: String)
    val params = Params(1, "hello")
    val result = foo.namedTupled(namedTuple(params.entityId, params.userId))
    assert(result)
  }

  test("namedTuple - variables") {
    val entityId = 1
    val userId = "hello"
    val result = namedTuple(entityId, userId)
    assertEquals(result.entityId, 1)
    assertEquals(result.userId, "hello")
  }

  test("namedTuple - field access") {
    case class Params(entityId: Int, userId: String)
    val params = Params(1, "hello")
    val result = namedTuple(params.entityId, params.userId)
    assertEquals(result.entityId, 1)
    assertEquals(result.userId, "hello")
  }

  test("namedTuple - duplicate names fail") {
    val errors = compileErrors("""
      import namedfunctions.syntax.*
      val entityId = 1
      namedTuple(entityId, entityId)
    """)
    assert(errors.contains("namedTuple: duplicate argument names: entityId"), errors)
  }

  test("namedTuple - literal argument fails") {
    val errors = compileErrors("""
      import namedfunctions.syntax.*
      namedTuple("hello")
    """)
    assert(errors.contains("namedTuple requires variable references or field accesses"), errors)
  }

  def fooMultiLists(entityId: Int)(userId: String): Boolean = true

  test("of - multi-parameter lists") {
    val f = fooMultiLists.named
    val result = f(entityId = 1)(userId = "hello")
    assert(result)
  }

  test("tupled - multi-parameter lists") {
    val g = fooMultiLists.namedTupled
    val result = g((entityId = 1))((userId = "hello"))
    assert(result)
  }

  test("untupled - named tuple function to named-parameter function") {
    val tupled: ((entityId: Int, userId: String)) => Boolean =
      t => t.entityId > 0 && t.userId.nonEmpty
    val f = tupled.namedUntupled
    val result = f(entityId = 1, userId = "hello")
    assert(result)
  }

  test("untupled roundtrip - tupled then untupled") {
    val g = foo.namedTupled
    val f = g.namedUntupled
    val result = f(entityId = 1, userId = "hello")
    assert(result)
  }

  // nameChecked tests

  test("nameChecked - matching names compiles and runs") {
    val entityId = 1
    val userId = "hello"
    val result = foo.nameChecked(entityId, userId)
    assert(result)
  }

  test("nameChecked - reorders arguments by name") {
    def bar(a: Int, b: String): String = s"$a-$b"
    val b = "hello"
    val a = 42
    val result = bar.nameChecked(b, a)
    assertEquals(result, "42-hello")
  }

  test("nameChecked - wrong name fails") {
    val errors = compileErrors("""
      import namedfunctions.syntax.*
      def foo(s: String, s2: String): Boolean = true
      val s = "hello"
      val x = "world"
      foo.nameChecked(s, x)
    """)
    assert(errors.contains("nameChecked: unexpected: x; missing: s2"), errors)
  }

  test("nameChecked - field access") {
    def bar(a: Int, b: String): String = s"$a-$b"
    case class Source(a: Int, b: String)
    val s = Source(42, "hello")
    val result = bar.nameChecked(s.a, s.b)
    assertEquals(result, "42-hello")
  }

  test("nameChecked - field access reorders") {
    def bar(a: Int, b: String): String = s"$a-$b"
    case class S1(b: String)
    case class S2(a: Int)
    val s1 = S1("hello")
    val s2 = S2(42)
    val result = bar.nameChecked(s1.b, s2.a)
    assertEquals(result, "42-hello")
  }

  test("nameChecked - literal argument fails") {
    val errors = compileErrors("""
      import namedfunctions.syntax.*
      def foo(s: String): Boolean = true
      foo.nameChecked("hello")
    """)
    assert(errors.contains("nameChecked requires variable references or field accesses"), errors)
  }

  test("nameChecked - multi-param lists") {
    val entityId = 1
    val userId = "hello"
    val result = fooMultiLists.nameChecked(entityId, userId)
    assert(result)
  }

  // applyProduct tests

  test("applyProduct - case class with matching fields") {
    case class Params(entityId: Int, userId: String)
    val params = Params(1, "hello")
    val result = foo.applyProduct(params)
    assert(result)
  }

  test("applyProduct - different field order") {
    def bar(a: Int, b: String): String = s"$a-$b"
    case class Params(b: String, a: Int)
    val params = Params("hello", 42)
    val result = bar.applyProduct(params)
    assertEquals(result, "42-hello")
  }

  test("applyProduct - case class with extra fields") {
    def bar(entityId: Int): Boolean = entityId > 0
    case class Params(entityId: Int, extra: String)
    val params = Params(1, "unused")
    val result = bar.applyProduct(params)
    assert(result)
  }

  test("applyProduct - missing field fails") {
    val errors = compileErrors("""
      import namedfunctions.syntax.*
      def foo(s: String, s2: String): Boolean = true
      case class Bar(s: String)
      foo.applyProduct(Bar("hello"))
    """)
    assert(errors.contains("has no field named 's2'"), errors)
  }

  test("applyProduct - multi-param lists") {
    case class Params(entityId: Int, userId: String)
    val params = Params(1, "hello")
    val result = fooMultiLists.applyProduct(params)
    assert(result)
  }

  // constructor tests

  test("applyProduct - case class constructor") {
    case class Foo(a: Int, b: String)
    case class Params(b: String, a: Int)
    val result = Foo.apply.applyProduct(Params("hello", 42))
    assertEquals(result, Foo(42, "hello"))
  }

  test("nameChecked - case class constructor") {
    case class Foo(a: Int, b: String)
    val a = 42
    val b = "hello"
    val result = Foo.apply.nameChecked(b, a)
    assertEquals(result, Foo(42, "hello"))
  }

  // function values don't carry parameter names

  test("applyProduct - function value fails (no parameter names)") {
    val errors = compileErrors("""
      import namedfunctions.syntax.*
      val f = (a: Int, b: String) => s"$a-$b"
      case class Params(a: Int, b: String)
      f.applyProduct(Params(42, "hello"))
    """)
    assert(errors.contains("Could not extract parameter names"), errors)
  }

  test("nameChecked - function value fails (no parameter names)") {
    val errors = compileErrors("""
      import namedfunctions.syntax.*
      val f = (a: Int, b: String) => s"$a-$b"
      val a = 42
      val b = "hello"
      f.nameChecked(a, b)
    """)
    assert(errors.contains("Could not extract parameter names"), errors)
  }

  // chaining / combination tests

  test("named then applyProduct") {
    def bar(a: Int, b: String): String = s"$a-$b"
    case class Params(b: String, a: Int)
    val result = bar.named.applyProduct(Params("hello", 42))
    assertEquals(result, "42-hello")
  }

  test("named then nameChecked") {
    def bar(a: Int, b: String): String = s"$a-$b"
    val a = 42
    val b = "hello"
    val result = bar.named.nameChecked(a, b)
    assertEquals(result, "42-hello")
  }

  test("namedTupled then namedUntupled then applyProduct") {
    def bar(a: Int, b: String): String = s"$a-$b"
    case class Params(b: String, a: Int)
    val result = bar.namedTupled.namedUntupled.applyProduct(Params("hello", 42))
    assertEquals(result, "42-hello")
  }

  test("namedTupled then namedUntupled then nameChecked") {
    def bar(a: Int, b: String): String = s"$a-$b"
    val b = "hello"
    val a = 42
    val result = bar.namedTupled.namedUntupled.nameChecked(b, a)
    assertEquals(result, "42-hello")
  }

  test("cats namedTupled - preserves named tuple labels") {
    import catssyntax.*

    val result =
      ((entityId = Option(1), userId = Option("hello"))).namedTupled

    assertEquals(result, Some((entityId = 1, userId = "hello")))
  }

  test("cats namedMapN - works with namedTupled functions") {
    import catssyntax.*

    val result =
      ((entityId = Option(1), userId = Option("hello"))).namedMapN(NamedFunctions.tupled(foo))

    assertEquals(result, Some(true))
  }

  test("cats namedParMapN - uses cats Parallel instances") {
    import catssyntax.*

    type V[A] = EitherNec[String, A]

    val result =
      ((entityId = "bad id".leftNec[Int], userId = "bad user".leftNec[String]))
        .namedParMapN(NamedFunctions.tupled(foo))

    assertEquals(result, Left(NonEmptyChain("bad id", "bad user")))
  }
}
