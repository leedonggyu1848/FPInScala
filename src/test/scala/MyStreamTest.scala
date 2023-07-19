import org.scalatest.funsuite.AnyFunSuite
import stream.MyStream

import scala.List
import scala.collection.IterableOnce.iterableOnceExtensionMethods

class MyStreamTest extends AnyFunSuite{
  test("5.1 toList") {
    assert(MyStream(1,2,3).toList == List(1,2,3))
    assert(MyStream().toList == Nil)
  }

  test("5.2 take and drop") {
    assert(MyStream(1,2,3,4,5).take(3).toList == List(1,2,3))
    assert(MyStream(1,2,3,4,5).drop(3).toList == List(4,5))
  }

  test("5.3 takeWhile") {
    assert(MyStream(1,2,3,4,5).takeWhile(_ < 3).toList == List(1,2))
    assert(MyStream(1,2,3,4,5).takeWhile(_ > 100).toList == List())
    assert(MyStream().takeWhile(_ => true).toList == List())
  }

  test("5.4 forAll") {
    assert(!MyStream(1,2,3,4,5).forAll(_%2 == 0))
    assert(MyStream(1,2,3,4,5).forAll(_ < 6))
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
    assert(MyStream(1,2,3).map(_ + 1).toList == List(2,3,4))
    assert(MyStream.empty[Int].map(_ + 1) == MyStream.empty)
  }

  test("5.7 filter test") {
    assert(MyStream(1,2,3,4,5).filter(_ % 2 == 0).toList == List(2,4))
    assert(MyStream(1,2,3,4,5).filter(_ % 2 == 1).toList == List(1,3,5))
    assert(MyStream.empty[Int].map(_ => true) == MyStream.empty)
  }

  test("5.7 append test") {
    assert(MyStream(1,2,3).append(MyStream(4,5,6)).toList == List(1,2,3,4,5,6))
    assert(MyStream.empty.append(MyStream(1,2,3)).toList == List(1,2,3))
    assert(MyStream(1,2,3).append(MyStream.empty).toList == List(1,2,3))
  }

  test("5.7 flatMap test") {
    assert(MyStream(1,2,3,4,5).flatMap(e => MyStream(e + 1)).toList == List(2,3,4,5,6))
    assert(MyStream.empty[Int].flatMap(e => MyStream(e + 1))== MyStream.empty)
  }
}
