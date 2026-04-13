//> using dep org.scalameta::munit::1.2.4

package namedfunctions

import syntax.*

class NamedFunctionsTest extends munit.FunSuite {

  def foo(entityId: Int, userId: String): Boolean = true

  test("of - named-parameter function") {
    val f = foo.named
    val result: Boolean = f(entityId = 1, userId = "hello")
    assert(result)
  }

  test("tupled - Function1 from named tuple") {
    val g = foo.namedTupled
    val result: Boolean = g((entityId = 1, userId = "hello"))
    assert(result)
  }

  def fooMultiLists(entityId: Int)(userId: String): Boolean = true

  test("of - multi-parameter lists") {
    val f = fooMultiLists.named
    val result: Boolean = f(entityId = 1)(userId = "hello")
    assert(result)
  }

  test("tupled - multi-parameter lists") {
    val g = fooMultiLists.namedTupled
    val result: Boolean = g((entityId = 1))((userId = "hello"))
    assert(result)
  }

  test("untupled - named tuple function to named-parameter function") {
    val tupled: ((entityId: Int, userId: String)) => Boolean =
      t => t.entityId > 0 && t.userId.nonEmpty
    val f = tupled.namedUntupled
    val result: Boolean = f(entityId = 1, userId = "hello")
    assert(result)
  }

  test("untupled roundtrip - tupled then untupled") {
    val g = foo.namedTupled
    val f = g.namedUntupled
    val result: Boolean = f(entityId = 1, userId = "hello")
    assert(result)
  }

  // nameChecked tests

  test("nameChecked - matching names compiles and runs") {
    val entityId = 1
    val userId = "hello"
    val result: Boolean = foo.nameChecked(entityId, userId)
    assert(result)
  }

  test("nameChecked - reorders arguments by name") {
    def bar(a: Int, b: String): String = s"$a-$b"
    val b = "hello"
    val a = 42
    val result: String = bar.nameChecked(b, a)
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
    val result: String = bar.nameChecked(s.a, s.b)
    assertEquals(result, "42-hello")
  }

  test("nameChecked - field access reorders") {
    def bar(a: Int, b: String): String = s"$a-$b"
    case class S1(b: String)
    case class S2(a: Int)
    val s1 = S1("hello")
    val s2 = S2(42)
    val result: String = bar.nameChecked(s1.b, s2.a)
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
    val result: Boolean = fooMultiLists.nameChecked(entityId, userId)
    assert(result)
  }

  // applyProduct tests

  test("applyProduct - case class with matching fields") {
    case class Params(entityId: Int, userId: String)
    val params = Params(1, "hello")
    val result: Boolean = foo.applyProduct(params)
    assert(result)
  }

  test("applyProduct - different field order") {
    def bar(a: Int, b: String): String = s"$a-$b"
    case class Params(b: String, a: Int)
    val params = Params("hello", 42)
    val result: String = bar.applyProduct(params)
    assertEquals(result, "42-hello")
  }

  test("applyProduct - case class with extra fields") {
    def bar(entityId: Int): Boolean = entityId > 0
    case class Params(entityId: Int, extra: String)
    val params = Params(1, "unused")
    val result: Boolean = bar.applyProduct(params)
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
    val result: Boolean = fooMultiLists.applyProduct(params)
    assert(result)
  }
}
