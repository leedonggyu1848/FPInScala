package ex8

import ex6.RNG
import ex6.RNG.{SimpleRNG, unit}
import ex8.Gen.*
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

def iterateCheck[A](gen: Gen[A])(f: A => Unit): Unit =
  val range = 1 to 1000
  var rng: RNG = SimpleRNG(1)
  range foreach { _ =>
    gen(rng) match {
      case (i, nextRng) =>
        f(i)
        rng = nextRng
    }
  }

class GenTest extends AnyFlatSpecLike with Matchers {
  behavior of "choose"
  it should "return range start to exclusive end" in {
    val start = 1
    val stop = 100
    var target = 0

    iterateCheck(Gen.choose(start, stop))(_ should (be >= start and be < stop))
    }

  behavior of "unit"
  it should "return unit Gen" in {
    val ten = 10

    iterateCheck(Gen.unit(ten))(_ shouldBe ten)
  }

  behavior of "boolean"
  it should "return true or false" in {
    var rng: RNG = SimpleRNG(1)

    1 to 100 map { e =>
      Gen.boolean(rng) match {
        case (bool, nextRng) =>
          rng = nextRng
           bool
      }
    } should contain allOf (true, false)
  }

  behavior of "listOfN"
  it should "return list that has length of N" in {
    val ten = 10

    iterateCheck(Gen.listOfN(ten, Gen.choose(1, 10))) { _.length shouldBe ten }
  }

  behavior of "listOfN from extension of Gen"
  it should "return list that has length of gen nub" in {
    val ten = 10
    val unitGen = Gen.unit(10)
    val sizeGen = Gen.choose(1,  10)

    iterateCheck(unitGen.listOfN(sizeGen)) { e =>
      e should contain only ten
      e.length should (be >= 1 and be < 10)
    }
  }

}
