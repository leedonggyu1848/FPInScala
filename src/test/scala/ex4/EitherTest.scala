package ex4

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import ex4.Either.{Left, Right}

class EitherTest extends AnyFlatSpecLike with Matchers {
  type TestType = ex4.Either[String, Int]
  val someMsg = "some message"
  val oneRightEither: TestType = Right(1)
  val twoRightEither: TestType = Right(2)
  val leftEither: TestType = Left(someMsg)

  behavior of "map"
  it should "map Right" in {
    oneRightEither.map(_ + 1) shouldEqual Right(2)
  }
  it should "not map Left" in {
    leftEither.map(_ => 1) shouldEqual leftEither
  }

  behavior of "flatMap"
  it should "flatMap Right" in {
    oneRightEither.flatMap(x => Right(x + 1)) shouldEqual Right(2)
  }
  it should "not flatMap Left" in {
    leftEither.flatMap(x => Right(x + 1)) shouldEqual leftEither
  }

  behavior of "orElse"
  it should "orElse Right" in {
    oneRightEither.orElse(twoRightEither) shouldEqual oneRightEither
  }
  it should "orElse Left" in {
    leftEither.orElse(twoRightEither) shouldEqual twoRightEither
  }

  behavior of "map2"
  it should "map2 Right" in {
    oneRightEither.map2(oneRightEither)(_ + _) shouldEqual twoRightEither
  }
  it should "not map2 Left" in {
    leftEither.map2(oneRightEither)(_ + _) shouldEqual leftEither
  }

  behavior of "sequence"
  it should "sequence Right" in {
    ex4.Either.sequence(List(oneRightEither, twoRightEither)) shouldEqual Right(List(1, 2))
  }
  it should "sequence Left" in {
    ex4.Either.sequence(List(oneRightEither, leftEither)) shouldEqual leftEither
  }

  behavior of "traverse"
  it should "traverse Right" in {
    ex4.Either.traverse(List(1, 2))(Right[String, Int] compose identity[Int]) shouldEqual Right(List(1, 2))
  }
}
