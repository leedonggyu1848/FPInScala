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

  test("option") {
    val zero = Monoid.optionMonoid.zero;
    assert(Monoid.optionMonoid.op(Option(1), Option(2)) == Option(1))
    assert(Monoid.optionMonoid.op(Option(1), zero) == Option(1))
    assert(Monoid.optionMonoid.op(zero, Option(1)) == Option(1))
    assert(Monoid.optionMonoid.op(zero, zero) == zero)
  }

  test("endo") {
    val zero = Monoid.endoMonoid[Int].zero;
    val f1: Int => Int = a => a + 1
    val f2: Int => Int = a => a * 2

    assert(Monoid.endoMonoid.op(f1, f2)(2) == 5)
    assert(Monoid.endoMonoid.op(f1, zero)(1) == 2)
    assert(Monoid.endoMonoid.op(zero, f1)(1) == 2)
    assert(Monoid.endoMonoid.op(zero, zero)(3) == 3)
  }

}
