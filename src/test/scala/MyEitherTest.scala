import org.scalatest.funsuite.AnyFunSuite

class MyEitherTest extends AnyFunSuite {
  private object fixtures {
    val someMsg = "something msg"
  }

  test("4.6 map test") {
    val rightEither: MyEither[String, Int] = MyRight(1)
    val leftEither: MyEither[String, Int] = MyLeft(fixtures.someMsg)

    val rightResult = rightEither.map(_ + 1)
    val leftResult = leftEither.map(_ + 1)

    assert(rightResult match {
      case MyRight(v) => v == 2
      case _ => false
    })
    assert(leftResult match {
      case MyLeft(v) => v == fixtures.someMsg
      case _ => false
    })
  }

  test("4.6 flatMap test") {
    val rightEither: MyEither[String, Int] = MyRight(1)
    val leftEither: MyEither[String, Int] = MyLeft(fixtures.someMsg)

    val rightResult = rightEither.flatMap(v => MyRight(v + 1))
    val leftResult = leftEither.flatMap(v => MyRight(v + 1))

    assert(rightResult match {
      case MyRight(v) => v == 2
      case _ => false
    })
    assert(leftResult match {
      case MyLeft(v) => v == fixtures.someMsg
      case _ => false
    })
  }

  test("4.6 orElse test") {
    val rightEither: MyEither[String, Int] = MyRight(1)
    val leftEither: MyEither[String, Int] = MyLeft(fixtures.someMsg)

    val defaultValue = 0
    val rightResult = rightEither.orElse(MyRight(defaultValue))
    val leftResult = leftEither.orElse(MyRight(defaultValue))

    assert(rightResult match {
      case MyRight(v) => v == 1
      case _ => false
    })
    assert(leftResult match {
      case MyRight(v) => v == defaultValue
      case _ => false
    })
  }

  test("4.6 map2 test") {
    val rightEither: MyEither[String, Int] = MyRight(1)
    val leftEither: MyEither[String, Int] = MyLeft(fixtures.someMsg)

    val rightResult = rightEither.map2(MyRight(2))(_ + _)
    val leftResult = leftEither.map2(MyRight(2))(_ + _)

    assert(rightResult match {
      case MyRight(v) => v == 3
      case _ => false
    })
    assert(leftResult match {
      case MyLeft(v) => v == fixtures.someMsg
      case _ => false
    })
  }

  test("4.7 traverse test") {
    val emts = List(1,2,3,4,5)

    val resultRight = MyEither.traverse(emts)(e => MyRight(e + 1))
    val resultLeft = MyEither.traverse(emts)(e => if (e % 2 == 1) MyRight(e) else MyLeft(fixtures.someMsg))

    assert(resultRight match {
      case MyRight(v) => v == List(2,3,4,5,6)
      case _ => false
    })
    assert(resultLeft match {
      case MyLeft(v) => v == fixtures.someMsg
      case _ => false
    })
  }

  test("4.7 sequence test") {
    val rightEmts = List(MyRight(1), MyRight(2), MyRight(3), MyRight(4), MyRight(5))
    val leftEmts = List(MyRight(1), MyRight(2), MyRight(3), MyLeft(fixtures.someMsg), MyRight(5))

    val resultRight = MyEither.sequence(rightEmts)
    val resultLeft = MyEither.sequence(leftEmts)

    assert(resultRight match {
      case MyRight(v) => v == List(1,2,3,4,5)
      case _ => false
    })
    assert(resultLeft match {
      case MyLeft(v) => v == fixtures.someMsg
      case _ => false
    })
  }
}
