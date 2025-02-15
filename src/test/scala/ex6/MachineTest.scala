package ex6

import ex6.Machine
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class MachineTest extends AnyFlatSpecLike with Matchers {
  behavior of "Machine"
  it should "getInterest" in {
    Machine(1).getInterest shouldBe (0, 1)
    Machine(0).getInterest shouldBe (0, 0)
    Machine(10).getInterest shouldBe  (0, 10)
    Machine(100).getInterest shouldBe (0, 100)
  }

  it should "simulate" in {
    Machine.simulateMachine(ex3.List(Coin)).run(Machine(1))._1 shouldBe (1, 1)
    Machine.simulateMachine(ex3.List(Coin)).run(Machine(0))._1 shouldBe (0, 0)
    Machine.simulateMachine(ex3.List(Coin, Coin)).run(Machine(1))._1 shouldBe (1, 1)

    Machine.simulateMachine(ex3.List(Turn)).run(Machine(1))._1 shouldBe (0, 1)
    Machine.simulateMachine(ex3.List(Turn)).run(Machine(0))._1 shouldBe (0, 0)
    Machine.simulateMachine(ex3.List(Turn, Turn)).run(Machine(2))._1 shouldBe (0, 2)

    Machine.simulateMachine(ex3.List(Coin, Turn)).run(Machine(2))._1 shouldBe (1, 1)
    Machine.simulateMachine(ex3.List(Coin, Turn, Coin)).run(Machine(2))._1 shouldBe (2, 1)
    Machine.simulateMachine(ex3.List(Coin, Turn, Coin, Turn)).run(Machine(2))._1 shouldBe (2, 0)
    Machine.simulateMachine(ex3.List(Coin, Turn, Coin, Turn, Coin)).run(Machine(2))._1 shouldBe (2, 0)

    Machine.simulateMachine(ex3.List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(5, true, 10))._1 shouldBe (14, 1)
  }


}
