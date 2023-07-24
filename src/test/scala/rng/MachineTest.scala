package rng

import org.scalatest.funsuite.AnyFunSuiteLike

class MachineTest extends AnyFunSuiteLike {
  test("test") {
    assert(Machine(1).getInterest == (0, 1))
    assert(Machine(0).getInterest == (0, 0))
    assert(Machine(10).getInterest == (0, 10))
    assert(Machine(100).getInterest == (0, 100))
  }

  test("coin") {
    assert(Machine.simulateMachine(List(Coin)).run(Machine(1))._1 == (1,1))
    assert(Machine.simulateMachine(List(Coin)).run(Machine(0))._1 == (0,0))
    assert(Machine.simulateMachine(List(Coin, Coin)).run(Machine(1))._1 == (1,1))
  }

  test("Turn") {
    assert(Machine.simulateMachine(List(Turn)).run(Machine(1))._1 == (0, 1))
    assert(Machine.simulateMachine(List(Turn)).run(Machine(0))._1 == (0,0))
    assert(Machine.simulateMachine(List(Turn,Turn)).run(Machine(2))._1 == (0, 2))
  }

  test("2 Coin Turn") {
    assert(Machine.simulateMachine(List(Coin, Turn)).run(Machine(2))._1 == (1, 1))
  }

  test("2 Coin Turn Coin") {
    assert(Machine.simulateMachine(List(Coin, Turn, Coin)).run(Machine(2))._1 == (2, 1))
  }

  test("2 Coin Turn Coin Turn") {
    assert(Machine.simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(2))._1 == (2, 0))
  }

  test("2 Coin Turn Coin Turn Coin") {
    assert(Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin)).run(Machine(2))._1 == (2, 0))
  }

  test("book example") {
    assert(Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(locked = true, 5, 10))._1 == (14, 1))
  }
}
