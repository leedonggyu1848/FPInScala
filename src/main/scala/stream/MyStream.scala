package stream

import stream.MyStream.{cons, empty}

case object MyEmpty extends MyStream[Nothing]
case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

sealed trait MyStream[+A] {
  def headOption: Option[A] = this match {
    case MyEmpty => None
    case MyCons(h, t) => Some(h())
  }

  def drop(n: Int): MyStream[A] = this match {
    case MyCons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def foldRight[B](z: => B)(f : (A, => B) => B): B = {
    this match {
      case MyCons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

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

  // 5.3
  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case MyCons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhileV2(p: A => Boolean): MyStream[A] =
    foldRight(empty: MyStream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // 5.6
  def headOptionV2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  def map[B](f: A => B): MyStream[B] =
    foldRight(empty[B])((e, acc) => cons(f(e), acc))

  def filter(f: A => Boolean): MyStream[A] =
    foldRight(empty[A])((e, acc) => if(f(e)) cons(e, acc) else acc)

  def append[A2 >: A](as: => MyStream[A2]): MyStream[A2] =
    foldRight(as)((e, acc) => cons(e, acc))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(empty[B])((e, acc) => f(e).append(acc))
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