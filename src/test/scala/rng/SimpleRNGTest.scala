package rng

import org.scalatest.funsuite.AnyFunSuiteLike

class SimpleRNGTest extends AnyFunSuiteLike {
  test("6.1 NegativeInt") {
    var rng: RNG = SimpleRNG(100L);
    for (_ <- 1 to 1000) {
      RNG.nonNegativeInt(rng) match {
        case (n, next) => assert(n > 0); rng = next; print(n)
      }
    }
  }
}
