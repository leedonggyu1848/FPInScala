package rng

import org.scalatest.funsuite.AnyFunSuiteLike

class SimpleRNGTest extends AnyFunSuiteLike {
  def rngGenerator[A](rng: RNG)(func: RNG.Rand[A]): LazyList[A] = {
    val (n, nextRng) = func(rng)
    n #:: rngGenerator[A](nextRng)(func)
  }

  private def fixed = new {
    def defaultRng: RNG = SimpleRNG(100)
    def defaultGenerator[A]: (RNG.Rand[A]) => LazyList[A] = rngGenerator(defaultRng)
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

  test("6.5 doubleViaMap") {
    fixed.defaultGenerator[Double](RNG.doubleViaMap)
      .map(e => assert(0 <= e && e < 1)).take(100).toList
  }

  test("6.6 map2") {
    val intDoubleStream = fixed.defaultGenerator[(Int, Double)](RNG.intDouble)
    val randIntDoubleStream = fixed.defaultGenerator[(Int, Double)](RNG.randIntDouble)
    val expr1 = intDoubleStream zip randIntDoubleStream map (e => assert(e._1 == e._2)) take 1000

    val doubleIntStream = fixed.defaultGenerator[(Double, Int)](RNG.doubleInt)
    val randDoubleIntStream = fixed.defaultGenerator[(Double, Int)](RNG.randDoubleInt)
    val expr2 = doubleIntStream zip randDoubleIntStream map (e => assert(e._1 == e._2)) take 1000

    (expr1.toList, expr2.toList)
  }

  test("6.7 ints") {
    assert(RNG.intsViaSequence(1000)(fixed.defaultRng)._1.length == 1000)
  }

  test("6.8 nonNegativeLessThen") {
    val targetStream = fixed.defaultGenerator(RNG.nonNegativeLessThan(6))
    targetStream.map(e => assert(0 <= 0 && e < 6)).take(1000).toList
  }

  test("6.9 flatMap") {
    val intDoubleStream = fixed.defaultGenerator[(Int, Double)](RNG.intDouble)
    val randIntDoubleStream = fixed.defaultGenerator[(Int, Double)](RNG.randIntDoubleViaFlatMap)
    val expr1 = intDoubleStream zip randIntDoubleStream map (e => assert(e._1 == e._2)) take 1000

    val doubleIntStream = fixed.defaultGenerator[(Double, Int)](RNG.doubleInt)
    val randDoubleIntStream = fixed.defaultGenerator[(Double, Int)](RNG.randDoubleIntViaFlatMap)
    val expr2 = doubleIntStream zip randDoubleIntStream map (e => assert(e._1 == e._2)) take 1000

    (expr1.toList, expr2.toList)
  }
}
