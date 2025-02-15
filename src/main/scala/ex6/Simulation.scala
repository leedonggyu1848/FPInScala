package ex6

import State.*

enum Input:
  case Coin
  case Turn

case class Machine(candies: Int, locked: Boolean = true, coins: Int = 0):
  def getInterest: (Int, Int) = (coins, candies)

  def update(input: Input): Machine = input match {
    case Input.Coin if locked && candies > 0 => Machine(candies, false, coins + 1)
    case Input.Turn if !locked && candies > 0 => Machine(candies - 1, true, coins)
    case _ => this
  }

object Machine:
  def update(input: Input)(m: Machine): Machine = m.update(input)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
      s <- State.get
    }  yield (s.coins, s.candies)