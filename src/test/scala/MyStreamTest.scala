import org.scalatest.funsuite.AnyFunSuite
import stream.MyStream

class MyStreamTest extends AnyFunSuite{
  test("5.1 toList") {
    assert(MyStream(1,2,3).toList == List(1,2,3))
    assert(MyStream().toList == Nil)
  }

  test("5.2 take and drop") {
    assert(MyStream(1,2,3,4,5).take(3).toList == List(1,2,3))
    assert(MyStream(1,2,3,4,5).drop(3).toList == List(4,5))
  }

}
