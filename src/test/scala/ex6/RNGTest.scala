package ex6

import ex6.RNG.{SimpleRNG, int, nonNegativeEven}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class RNGTest extends AnyFlatSpecLike with Matchers {
  def rngGenerator[A](rng: RNG)(func: RNG.Rand[A]): LazyList[A] =
    val (n, nextRng) = func(rng)
    n #:: rngGenerator(nextRng)(func)

  val defaultRng = SimpleRNG(42)
  def defaultGenerator[A] = rngGenerator[A](defaultRng)


  behavior of "nonNegativeInt"
  it should "return a nonNegative int" in {
    defaultGenerator(RNG.nonNegativeEven) take 100 foreach (_ should be >= 0)
  }

  behavior of "double"
  it should "return a double between 0 and 1" in {
    defaultGenerator(RNG.double) take 100 foreach(n => {
      n should be >= 0.0
      n should be < 1.0
    })
  }

  behavior of "ints"
  it should "return a list of ints" in {
    RNG.ints(100)(defaultRng)._1.length shouldBe 100
  }

  behavior of "doubleViaMap"
  it should "return a double between 0 and 1" in {
    defaultGenerator(RNG.doubleViaMap) take 100 foreach (n => {
      n should be >= 0.0
      n should be < 1.0
    })
  }

  behavior of "map2"
  it should "combine two functions" in {
    val newIntDouble = RNG.map2(RNG.int, RNG.double)((_, _))
    rngGenerator(defaultRng)(newIntDouble)
      `take` 100
      `foreach` (n => {
        n._1 should be >= Int.MinValue
        n._1 should be < Int.MaxValue
        n._2 should be >= 0.0
        n._2 should be < 1.0
      })
  }

  behavior of "intsViaSequence"
  it should "return a list of ints and that using sequence" in {
    RNG.intsViaSequence(100)(defaultRng)._1.length shouldBe 100
  }

  behavior of "nonNegativeLessThan"
  it should "return a nonNegative int less than n" in {
    defaultGenerator(RNG.nonNegativeLessThan(100)) take 100 foreach (_ should be >= 0)
  }
}
