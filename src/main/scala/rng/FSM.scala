package rng

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def getInterest: (Int, Int) = (coins, candies)

  def update(input: Input): Machine = input match {
    case Coin if locked && candies > 0 => Machine(locked = false, candies, coins+1)
    case Turn if !locked && candies > 0 => Machine(locked = true, candies-1, coins)
    case _  => this
  }

}

object Machine {
  def apply(n: Int): Machine = {
    Machine(locked = true, n, 0)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State(e => {
      val s = inputs
        .foldLeft[Machine](e)((acc, v) => acc.update(v))
      (s.getInterest, s)
    })
  }
}

/*
  두 종류의 입력이 있다. 동전과 손잡이
  사탕이 몇 개나 남았는지, 동전이 몇 개나 들어있는지 추적한다.
  규칙은 다음과 같다.
    잠겨진 판매기에 동전을 넣으면 사탕이 남아있는 경우 잠김이 풀린다.
    풀린 판매기의 손잡이를 돌리면 사탕이 나오고 판매기가 잠긴다.
    잠긴 판매기의 손잡이를 돌리거나 풀린 판매기에 동전을 넣으면 아무일도 생기지 않는다.
    사탕이 없는 판매기는 모든 입력을 무시한다.
 */
