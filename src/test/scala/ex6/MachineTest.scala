package ex6

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import Input.*

class MachineTest extends AnyFlatSpecLike with Matchers {
  behavior of "Machine"
  it should "getInterest" in {
    Machine(1).getInterest shouldBe (0, 1)
    Machine(0).getInterest shouldBe (0, 0)
    Machine(10).getInterest shouldBe  (0, 10)
    Machine(100).getInterest shouldBe (0, 100)
  }

  it should "simulate" in {
    Machine.simulateMachine(List(Coin))(Machine(1))._1 shouldBe (1, 1)
    Machine.simulateMachine(List(Coin))(Machine(0))._1 shouldBe (0, 0)
    Machine.simulateMachine(List(Coin, Coin))(Machine(1))._1 shouldBe (1, 1)

    Machine.simulateMachine(List(Turn))(Machine(1))._1 shouldBe (0, 1)
    Machine.simulateMachine(List(Turn))(Machine(0))._1 shouldBe (0, 0)
    Machine.simulateMachine(List(Turn, Turn))(Machine(2))._1 shouldBe (0, 2)

    Machine.simulateMachine(List(Coin, Turn))(Machine(2))._1 shouldBe (1, 1)
    Machine.simulateMachine(List(Coin, Turn, Coin))(Machine(2))._1 shouldBe (2, 1)
    Machine.simulateMachine(List(Coin, Turn, Coin, Turn))(Machine(2))._1 shouldBe (2, 0)
    Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin))(Machine(2))._1 shouldBe (2, 0)

    Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))(Machine(5, true, 10))._1 shouldBe (14, 1)
  }


}
