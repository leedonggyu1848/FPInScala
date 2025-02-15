package ex5

import ex4.Option
import ex4.Option.*

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class StreamTest extends AnyFlatSpecLike with Matchers {
  behavior of "toList"
  it should "convert a stream to a list" in {
    ex5.Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
    ex5.Stream().toList shouldBe Nil
  }

  behavior of "take"
  it should "take the first n elements of a stream" in {
    ex5.Stream(1, 2, 3).take(2).toList shouldBe List(1, 2)
    ex5.Stream(1, 2, 3).take(0).toList shouldBe Nil
    ex5.Stream(1, 2, 3).take(4).toList shouldBe List(1, 2, 3)
  }

  behavior of "drop"
  it should "drop the first n elements of a stream" in {
    ex5.Stream(1, 2, 3).drop(2).toList shouldBe List(3)
    ex5.Stream(1, 2, 3).drop(0).toList shouldBe List(1, 2, 3)
    ex5.Stream(1, 2, 3).drop(4).toList shouldBe Nil
  }

  behavior of "takeWhile"
  it should "take elements while the predicate is true" in {
    ex5.Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList shouldBe List(1, 2)
    ex5.Stream(1, 2, 3, 4, 5).takeWhile(_ > 100).toList shouldBe Nil
    ex5.Stream().takeWhile(_ => true).toList shouldBe Nil
  }

  behavior of "forAll"
  it should "return true if all elements satisfy the predicate" in {
    ex5.Stream(1, 2, 3, 4, 5).forAll(_ % 2 == 0) shouldBe false
    ex5.Stream(1, 2, 3, 4, 5).forAll(_ < 6) shouldBe true
  }

  behavior of "takeWhileViaFoldRight"
  it should "take elements while the predicate is true" in {
    ex5.Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ < 3).toList shouldBe List(1, 2)
    ex5.Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ > 100).toList shouldBe Nil
    ex5.Stream().takeWhileViaFoldRight(_ => true).toList shouldBe Nil
  }

  behavior of "map"
  it should "map elements" in {
    ex5.Stream(1, 2, 3).map(_ + 1).toList shouldBe List(2, 3, 4)
    ex5.Stream().map(_ => 1).toList shouldBe Nil
  }

  behavior of "filter"
  it should "filter elements" in {
    ex5.Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList shouldBe List(2, 4)
    ex5.Stream(1, 2, 3, 4, 5).filter(_ => false).toList shouldBe Nil
    ex5.Stream().filter(_ => true).toList shouldBe Nil
  }

  behavior of "append"
  it should "append two streams" in {
    ex5.Stream(1, 2, 3).append(ex5.Stream(4, 5, 6)).toList shouldBe List(1, 2, 3, 4, 5, 6)
    ex5.Stream().append(ex5.Stream(1, 2, 3)).toList shouldBe List(1, 2, 3)
    ex5.Stream(1, 2, 3).append(ex5.Stream()).toList shouldBe List(1, 2, 3)
  }

  behavior of "flatMap"
  it should "flat map elements" in {
    ex5.Stream(1, 2, 3, 4, 5).flatMap(e => ex5.Stream(e + 1)).toList shouldBe List(2, 3, 4, 5, 6)
    ex5.Stream[Int]().flatMap(e => ex5.Stream(e + 1)).toList shouldBe Nil
  }

  behavior of "constant"
  it should "return an infinite stream of a given value" in {
    ex5.Stream.constant(1).take(5).toList shouldBe List(1, 1, 1, 1, 1)
    ex5.Stream.constant(1).take(0).toList shouldBe Nil
  }

  behavior of "from"
  it should "return an infinite stream of integers starting from a given value" in {
    ex5.Stream.from(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  behavior of "fibs"
  it should "return an infinite stream of fibonacci numbers" in {
    ex5.Stream.fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  behavior of "unfold"
  it should "return an infinite stream of integers starting from a given value" in {
    ex5.Stream.unfold(1)(e => Some((e, e + 1))).take(5).toList shouldBe LazyList.unfold(1)(e => Some((e, e + 1))).take(5).toList
  }

  behavior of "fibsViaUnfold"
  it should "return an infinite stream of fibonacci numbers" in {
    ex5.Stream.fibsViaUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  behavior of "fromViaUnfold"
  it should "return an infinite stream of integers starting from a given value" in {
    ex5.Stream.fromViaUnfold(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  behavior of "constantViaUnfold"
  it should "return an infinite stream of a given value" in {
    ex5.Stream.constantViaUnfold(1).take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  behavior of "mapViaUnfold"
  it should "map elements" in {
    ex5.Stream(1, 2, 3).mapViaUnfold(_ + 1).toList shouldBe List(2, 3, 4)
    ex5.Stream().mapViaUnfold(_ => 1).toList shouldBe Nil
  }

  behavior of "takeViaUnfold"
  it should "take the first n elements of a stream" in {
    ex5.Stream(1, 2, 3).takeViaUnfold(2).toList shouldBe List(1, 2)
    ex5.Stream(1, 2, 3).takeViaUnfold(0).toList shouldBe Nil
    ex5.Stream().takeViaUnfold(10).toList shouldBe Nil
  }

  behavior of "takeWhileViaUnfold"
  it should "take elements while the predicate is true" in {
    ex5.Stream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ < 3).toList shouldBe List(1, 2)
    ex5.Stream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ > 100).toList shouldBe Nil
    ex5.Stream().takeWhileViaUnfold(_ => true).toList shouldBe Nil
  }

  behavior of "zipWith"
  it should "zip two streams with a given function" in {
    ex5.Stream(1, 2, 3, 4, 5).zipWith(ex5.Stream(1, 2, 3, 4, 5))(_ + _).toList shouldBe List(2, 4, 6, 8, 10)
    ex5.Stream(1, 2, 3).zipWith(ex5.Stream(1, 2, 3, 4, 5))(_ + _).toList shouldBe List(2, 4, 6)
    ex5.Stream(1, 2, 3, 4, 5).zipWith(ex5.Stream(1, 2, 3))(_ + _).toList shouldBe List(2, 4, 6)
    ex5.Stream(1, 2, 3).zipWith(ex5.Stream.empty[Int])(_ + _).toList shouldBe Nil
    ex5.Stream.empty[Int].zipWith(ex5.Stream(1, 2, 3))(_ + _).toList shouldBe Nil
  }

  behavior of "zipAll"
  it should "zip two streams with a given function" in {
    val add: ((ex4.Option[Int], ex4.Option[Int]), => List[Int]) => List[Int] = (dt, acc) => dt match
      case (Some(a), Some(b)) => a + b :: acc
      case (None, Some(b)) => b :: acc
      case (Some(a), None) => a :: acc
      case _ => acc

    ex5.Stream(1, 2, 3).zipAll(ex5.Stream(1, 2, 3)).foldRight(List.empty: List[Int])(add) shouldBe List(2, 4, 6)
    ex5.Stream(1, 2).zipAll(ex5.Stream(1, 2, 3)).foldRight(List.empty: List[Int])(add) shouldBe List(2, 4, 3)
    ex5.Stream(1, 2, 3).zipAll(ex5.Stream(1, 2)).foldRight(List.empty: List[Int])(add) shouldBe List(2, 4, 3)
    ex5.Stream.empty[Int].zipAll(ex5.Stream.empty[Int]).foldRight(List.empty: List[Int])(add) shouldBe Nil
  }

  behavior of "startsWith"
  it should "return true if a stream starts with another stream" in {
    ex5.Stream(1, 2, 3).startsWith(ex5.Stream(1, 2)) shouldBe true
    ex5.Stream(1, 2, 3).startsWith(ex5.Stream(1, 2, 3)) shouldBe true
    ex5.Stream(1, 2, 3).startsWith(ex5.Stream(1, 2, 3, 4)) shouldBe false
    ex5.Stream(1, 2, 3).startsWith(ex5.Stream.empty[Int]) shouldBe true
    ex5.Stream.empty[Int].startsWith(ex5.Stream.empty[Int]) shouldBe true
    ex5.Stream.empty[Int].startsWith(ex5.Stream(1, 2, 3)) shouldBe false
  }

  behavior of "tails"
  it should "return all tails of a stream" in {
    ex5.Stream(1, 2, 3).tails.map(_.toList).toList shouldBe List(List(1, 2, 3), List(2, 3), List(3), Nil)
    ex5.Stream.empty[Int].tails.map(_.toList).toList shouldBe List(Nil)
  }

}

