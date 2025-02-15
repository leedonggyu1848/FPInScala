package ex4

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A]: 
  // 4.1
  def map[B](f: A => B): Option[B] = this match
    case Some(v) => Some(f(v))
    case None => None

  def getOrElse[B >: A](default: => B): B = this match
    case Some(v) => v
    case None => default

  def flatMap[B](f: A => Option[B]): Option[B] =
    this map f getOrElse None


  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map { Some(_) } getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    this flatMap { a => if f(a) then Some(a) else None }

object Option:
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  // 4.2
  private def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
      mean(xs map { x => math.pow(x - m, 2) } )
    }

  // 4.3
  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa flatMap { a => ob map { b => f(a, b) } }

  // 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil): Option[List[A]]) { (oa, acc) =>
      map2(oa, acc)(_ :: _)
    }

  // 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]]) { (a, acc) =>
      map2(f(a), acc)(_ :: _)
    }

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)
