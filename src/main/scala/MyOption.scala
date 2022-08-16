sealed trait MyOption[+A] {

  // 4.1
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(v) => MySome(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(v) => v
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    this.map(f).getOrElse(MyNone)

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    map(MySome(_)).getOrElse(ob)

  def filter(f: A => Boolean): MyOption[A] = {
    flatMap(x => if(f(x)) MySome(x) else MyNone )
  }
}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

object MyOption {
  // 4.2
  def mean(xs: Seq[Double]): MyOption[Double] = {
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  def Try[A](a: => A): MyOption[A] =
    try MySome(a)
    catch { case e: Exception => MyNone }

  // 4.3
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  // 4.4
  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] =
    a.foldRight[MyOption[List[A]]](MySome(Nil))((x, z) => map2(x, z)(_::_))

  //4.5
  def traverse[A,B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    a.foldRight[MyOption[List[B]]](MySome(Nil))((x, z) => map2(f(x), z)(_::_))

  def sequenceViaTraverse[A](a: List[MyOption[A]]): MyOption[List[A]] =
    traverse(a)(x=>x)
}

