package stream

import stream.MyStream.{cons, empty}

case object MyEmpty extends MyStream[Nothing]
case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

sealed trait MyStream[+A] {
  def headOption: Option[A] = this match {
    case MyEmpty => None
    case MyCons(h, t) => Some(h())
  }

  // 5.1
  def toList: List[A] = this match {
    case MyEmpty => Nil
    case MyCons(h, t) => h() :: t().toList
  }

  // 5.2
  def take(n: Int): MyStream[A] = this match {
    case MyCons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case MyCons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): MyStream[A] = this match {
    case MyCons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }
}

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    MyCons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}