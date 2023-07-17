// 4.6
sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyLeft(v) => MyLeft(v)
    case MyRight(v) => MyRight(f(v))
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(v) => MyLeft(v)
    case MyRight(v) => f(v)
  }

  def orElse[EE>:E, B>:A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(_) => b
    case MyRight(v) => MyRight(v)
  }

  def map2[EE>:E, B, C](b: MyEither[EE, B])(f: (A,B) => C): MyEither[EE, C] =
    this.flatMap(x => b.map(y => f(x, y)))
}
case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]

object MyEither {
  def Try[A](a: => A): MyEither[Exception, A] =
    try MyRight(a)
    catch { case e: Exception => MyLeft(e)}

  //4.7
  def traverse[E, A, B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    es.foldRight[MyEither[E, List[B]]](MyRight(Nil))((x,z) => f(x).map2(z)(_ :: _))
  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
    traverse(es)(x => x)
}