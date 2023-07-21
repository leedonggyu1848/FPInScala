import org.scalatest.funsuite.AnyFunSuite
import stream.MyStream

import scala.List
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.View.Empty

class MyStreamTest extends AnyFunSuite {
  test("5.1 toList") {
    assert(MyStream(1, 2, 3).toList == List(1, 2, 3))
    assert(MyStream().toList == Nil)
  }

  test("5.2 take and drop") {
    assert(MyStream(1, 2, 3, 4, 5).take(3).toList == List(1, 2, 3))
    assert(MyStream(1, 2, 3, 4, 5).drop(3).toList == List(4, 5))
  }

  test("5.3 takeWhile") {
    assert(MyStream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList == List(1, 2))
    assert(MyStream(1, 2, 3, 4, 5).takeWhile(_ > 100).toList == List())
    assert(MyStream().takeWhile(_ => true).toList == List())
  }

  test("5.4 forAll") {
    assert(!MyStream(1, 2, 3, 4, 5).forAll(_ % 2 == 0))
    assert(MyStream(1, 2, 3, 4, 5).forAll(_ < 6))
  }

  test("5.5 takeWhile with fordRight") {

    assert(MyStream(1, 2, 3, 4, 5).takeWhileV2(_ < 3).toList == List(1, 2))
    assert(MyStream(1, 2, 3, 4, 5).takeWhileV2(_ > 100).toList == List())
    assert(MyStream().takeWhileV2(_ => true).toList == List())
  }

  test("5.6 headOption with foldRight") {
    assert(MyStream(1, 2, 3).headOptionV2.contains(1))
    assert(MyStream(1).headOptionV2.contains(1))
    assert(MyStream().headOptionV2.isEmpty)
  }

  test("5.7 map test") {
    assert(MyStream(1, 2, 3).map(_ + 1).toList == List(2, 3, 4))
    assert(MyStream.empty[Int].map(_ + 1) == MyStream.empty)
  }

  test("5.7 filter test") {
    assert(MyStream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2, 4))
    assert(MyStream(1, 2, 3, 4, 5).filter(_ % 2 == 1).toList == List(1, 3, 5))
    assert(MyStream.empty[Int].map(_ => true) == MyStream.empty)
  }

  test("5.7 append test") {
    assert(MyStream(1, 2, 3).append(MyStream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
    assert(MyStream.empty.append(MyStream(1, 2, 3)).toList == List(1, 2, 3))
    assert(MyStream(1, 2, 3).append(MyStream.empty).toList == List(1, 2, 3))
  }

  test("5.7 flatMap test") {
    assert(MyStream(1, 2, 3, 4, 5).flatMap(e => MyStream(e + 1)).toList == List(2, 3, 4, 5, 6))
    assert(MyStream.empty[Int].flatMap(e => MyStream(e + 1)) == MyStream.empty)
  }

  test("5.8 constant test") {
    assert(MyStream.constant(1).take(10).toList.size == 10)
  }

  test("5.9 from test") {
    assert(MyStream.from(1).take(5).toList == List(1, 2, 3, 4, 5))
  }

  test("5.10 fibs test") {
    assert(MyStream.fibs.take(5).toList == List(0, 1, 1, 2, 3))
  }

  test("5.11 unfold test") {
    val func = (x: Int) =>
      if (x < 10) Some((x, x + 1)) else None

    assert(MyStream.unfold(1)(func).toList == LazyList.unfold(1)(func).toList)
    assert(MyStream.unfold(1)(func).take(3).toList == LazyList.unfold(1)(func).take(3).toList)
  }

  test("5.12 fibsViaUnfold") {
    assert(MyStream.fibsViaUnfold.take(5).toList == List(0, 1, 1, 2, 3))
  }

  test("5.12 fromViaUnFold") {
    assert(MyStream.fromViaUnfold(1).take(5).toList == List(1, 2, 3, 4, 5))
  }

  test("5.12 constantViaUnfold") {
    assert(MyStream.constantViaUnfold(1).take(10).toList.size == 10)
  }

  test("5.12 onesViaUnfold") {
    assert(MyStream.onesViaUnfold.take(10).toList.size == 10)
  }

  test("5.13 mapViaUnfold") {
    assert(MyStream.mapViaUnfold(MyStream(1, 2, 3))(_ + 1).toList == List(2, 3, 4))
    assert(MyStream.mapViaUnfold(MyStream.empty[Int])(_ + 1).toList == List.empty)
  }

  test("5.13 takeViaUnfold") {
    assert(MyStream.takeViaUnfold(MyStream(1, 2, 3, 4, 5))(3).toList == List(1, 2, 3))
    assert(MyStream.takeViaUnfold(MyStream(1, 2, 3, 4, 5))(0).toList == List.empty)
    assert(MyStream.takeViaUnfold(MyStream())(10).toList == List.empty)
  }

  test("5.13 zipWithViaUnfold") {
    assert(MyStream.zipWithViaUnfold
    (MyStream(1, 2, 3, 4, 5), MyStream(1, 2, 3, 4, 5))
    (_ + _).toList == List(2, 4, 6, 8, 10))

    assert(MyStream.zipWithViaUnfold
    (MyStream(1, 2, 3), MyStream(1, 2, 3, 4, 5))
    (_ + _).toList == List(2, 4, 6))

    assert(MyStream.zipWithViaUnfold
    (MyStream(1, 2, 3, 4, 5), MyStream(1, 2, 3))
    (_ + _).toList == List(2, 4, 6))

    assert(MyStream.zipWithViaUnfold
    (MyStream(1, 2, 3), MyStream.empty)
    (_ + _).toList == List.empty)

    assert(MyStream.zipWithViaUnfold
    (MyStream(1, 2, 3), MyStream.empty)
    (_ + _).toList == List.empty)
  }

  test("5.13 zipAllViaUnfold") {
    val add: ((Option[Int], Option[Int]), => List[Int]) => List[Int] = (dt, acc) => {
        dt match {
          case (Some(a), Some(b)) => a + b :: acc
          case (None, Some(b)) => b :: acc
          case (Some(a), None) => a :: acc
          case _ => acc
        }
      }
    assert(MyStream.zipAllViaUnfold(MyStream(1, 2, 3), MyStream(1, 2, 3))
      .foldRight(List.empty: List[Int])(add) == List(2, 4, 6))

    assert(MyStream.zipAllViaUnfold(MyStream(1, 2), MyStream(1, 2, 3))
      .foldRight(List.empty: List[Int])(add) == List(2, 4, 3))

    assert(MyStream.zipAllViaUnfold(MyStream(1, 2, 3), MyStream(1, 2))
      .foldRight(List.empty: List[Int])(add) == List(2, 4, 3))

    assert(MyStream.zipAllViaUnfold(MyStream.empty, MyStream.empty)
      .foldRight(List.empty: List[Int])(add) == List.empty)
  }
}
