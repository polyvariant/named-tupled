//> using dep org.scalameta::munit::1.2.4

package namedtupled

import syntax.*

class NamedTupledTest extends munit.FunSuite {

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

  // test("of - multi-parameter lists") {
  //   val f = fooMultiLists.named
  //   val result: Boolean = f(entityId = 1)(userId = "hello")
  //   assert(result)
  // }

  // test("tupled - multi-parameter lists") {
  //   val g = fooMultiLists.namedTupled
  //   val result: Boolean = g((entityId = 1))((userId = "hello"))
  //   assert(result)
  // }
}
