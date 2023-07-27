package integration

import org.scalatest.funsuite.AnyFunSuiteLike

class MonoidTest extends AnyFunSuiteLike {

  test("intAddition") {
    val zero = Monoid.intAddition.zero
    assert(Monoid.intAddition.op(1, 2) == 3)
    assert(Monoid.intAddition.op(zero, 300) == 300)
    assert(Monoid.intAddition.op(zero, 0) == 0)
    assert(Monoid.intAddition.op(zero, zero) == zero)
  }

  test("intMultiplication") {
    val zero = Monoid.intMultiplication.zero
    assert(Monoid.intMultiplication.op(10, 20) == 200)
    assert(Monoid.intMultiplication.op(zero, 0) == 0)
    assert(Monoid.intMultiplication.op(zero, 2000) == 2000)
    assert(Monoid.intMultiplication.op(zero, zero) == zero)
  }

  test("booleanOr") {
    val zero = Monoid.booleanOr.zero
    assert(Monoid.booleanOr.op(true, false))
    assert(Monoid.booleanOr.op(true, true))
    assert(!Monoid.booleanOr.op(false, false))
    assert(!Monoid.booleanOr.op(zero, false))
    assert(Monoid.booleanOr.op(zero, true))
  }

  test("booleanAnd") {
    val zero = Monoid.booleanAnd.zero
    assert(Monoid.booleanAnd.op(true, true))
    assert(!Monoid.booleanAnd.op(true, false))
    assert(!Monoid.booleanAnd.op(false, false))
    assert(Monoid.booleanAnd.op(zero, true))
    assert(!Monoid.booleanAnd.op(zero, false))

  }

}
