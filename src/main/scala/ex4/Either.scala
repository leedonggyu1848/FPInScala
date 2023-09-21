package ex4

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  // 4.6
  def map[B](f: A => B): Either[E, B] = this match
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(value) => Left(value)
    case Right(value) => f(value)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(value) => b
    case Right(value) => Right(value)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap { x => b map { y => f(x, y) } }

object Either:
  // 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]]) { (a, acc) => f(a).map2(acc)(_ :: _) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)


