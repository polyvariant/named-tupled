//> using dep org.scalameta::munit::1.1.0

package namedtupled

class NamedTupledTest extends munit.FunSuite {

  def foo(entityId: Int, userId: String): Boolean = true

  test("of — named-parameter function") {
    val f = NamedTupled.of(foo)
    val result: Boolean = f(entityId = 1, userId = "hello")
    assert(result)
  }

  test("tupled — Function1 from named tuple") {
    val g = NamedTupled.tupled(foo)
    val result: Boolean = g((entityId = 1, userId = "hello"))
    assert(result)
  }

  test("named field access on the tuple key type") {
    val extractUserId: ((entityId: Int, userId: String)) => String = _.userId
    assertEquals(extractUserId((entityId = 1, userId = "hello")), "hello")
  }

}
