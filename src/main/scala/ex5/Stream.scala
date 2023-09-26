package ex5

import ex5.Stream.*

enum Stream[+A]:
  case Cons(h: () => A, t: () => Stream[A])
  case Empty extends Stream[Nothing]

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, _) => Some(h())

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // 5.1
  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  // 5.2
  def take(n: Int): Stream[A] = this match
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty

  def drop(n: Int): Stream[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if p(a) then cons(a, b) else empty)

  // 5.6
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if p(a) then cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  // 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case _ => None
    }

  // 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    this zipAll s takeWhile {
      case (_, Some(_)) => true
      case _ => false
    } forAll {
      case (a, b) => a == b
    }

  // 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(Stream(empty))



object Stream:
  def cons[A](h: => A, t: => Stream[A]) =
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  def ones: Stream[Int] =
    cons(1, ones)

  // 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // 5.10
  def fibs: Stream[Int] =
    def inner(a: Int, b: Int): Stream[Int] =
      cons(a, inner(b, a + b))
    inner(0, 1)

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty

  // 5.12
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1))((a, b) => Some((a, (b, a + b))))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

