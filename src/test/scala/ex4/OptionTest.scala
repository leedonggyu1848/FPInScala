package ex4

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import ex4.Option.{None, Some}

class OptionTest extends AnyFlatSpecLike with Matchers {
  val some: ex4.Option[Int] = Some(10)
  val none: ex4.Option[Int] = None

  behavior of "map"
  it should "map Some" in {
    some.map(_ + 1) shouldBe Some(11)
  }
  it should "map None" in {
    none.map(_ + 1) shouldBe None
  }

  behavior of "flatMap"
  it should "flatMap Some" in {
    some.flatMap(Some(_)) shouldBe some
  }
  it should "flatMap None" in {
    none.flatMap(Some(_)) shouldBe None
  }

  behavior of "getOrElse"
  it should "getOrElse Some" in {
    some.getOrElse(0) shouldBe 10
  }
  it should "getOrElse None" in {
    none.getOrElse(0) shouldBe 0
  }

  behavior of "orElse"
  it should "orElse Some" in {
    some.orElse(Some(0)) shouldBe some
  }
  it should "orElse None" in {
    none.orElse(Some(0)) shouldBe Some(0)
  }

  behavior of "filter"
  it should "filter Some" in {
    some.filter(_ => true) shouldBe some
  }
  it should "filter None" in {
    some.filter(_ => false) shouldBe None
  }

  behavior of "variance"
  it should "calculate variance" in {
    ex4.Option.variance(Seq(1, 2, 3, 4, 5)) shouldBe Some(2.0)
  }
  it should "calculate variance for empty sequence" in {
    ex4.Option.variance(Seq()) shouldBe None
  }

  behavior of "map2"
  it should "map2 Some" in {
    ex4.Option.map2(Some(1), Some(2))(_ + _) shouldBe Some(3)
  }
  it should "map2 None" in {
    ex4.Option.map2[Int, Int, Int](Some(1), None)(_ + _) shouldBe None
    ex4.Option.map2[Int, Int, Int](None, Some(1))(_ + _) shouldBe None
  }

  behavior of "sequence"
  it should "sequence Some" in {
    ex4.Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    ex4.Option.sequenceViaTraverse(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  }
  it should "sequence None" in {
    ex4.Option.sequenceViaTraverse(List(Some(1), None, Some(3))) shouldBe None
  }

  behavior of "traverse"
  it should "traverse Some" in {
    ex4.Option.traverse(List(1, 2, 3))(Some(_)) shouldBe Some(List(1, 2, 3))
  }
  it should "traverse None" in {
    ex4.Option.traverse(List(1, 2, 3))(i => if (i == 2) None else Some(i)) shouldBe None
  }
}
