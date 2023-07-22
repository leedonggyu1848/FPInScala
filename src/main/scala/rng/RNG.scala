package rng

import scala.annotation.tailrec

sealed trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0XFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, next) if n < 0 => nonNegativeInt(next)
    case ret => ret
  }
}