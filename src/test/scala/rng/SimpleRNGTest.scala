package rng

import org.scalatest.funsuite.AnyFunSuiteLike

class SimpleRNGTest extends AnyFunSuiteLike {
  def rngGenerator[A](rng: RNG)(func: Function[RNG, (A, RNG)]): LazyList[A] = {
    val (n, nextRng) = func(rng)
    n #:: rngGenerator[A](nextRng)(func)
  }

  private def fixed = new {
    def defaultRng: RNG = SimpleRNG(100)
    def defaultGenerator[A]: (Function[RNG, (A, RNG)]) => LazyList[A] = rngGenerator(defaultRng)
  }

  test("6.1 NegativeInt") {
    fixed.defaultGenerator[Int](RNG.nonNegativeInt)
      .map(e => assert(e >= 0)).take(1000).toList
  }

  test("6.2 double") {
    fixed.defaultGenerator[Double](RNG.double)
      .map(e => assert(0 <= e && e < 1)).take(100).toList
  }

  test("6.3 ints") {
    assert(RNG.ints(100)(fixed.defaultRng)._1.length == 100)
  }
}
