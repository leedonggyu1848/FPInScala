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
}
