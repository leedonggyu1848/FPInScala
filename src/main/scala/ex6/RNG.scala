package ex6

sealed trait RNG:
  def nextInt: (Int, RNG)

object RNG:
  case class SimpleRNG(seed: Long) extends RNG:
    override def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0XFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)


  /**
   * 이것은 함수이다. RNG를 받아서 다음값 (A)와 다음 RNG를 반환한다.
   */
  type Rand[+A] = State[RNG, A] // RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  /**
   * 특정 값을 받고
   * 이후 RNG값을 받아서 다음값과 다음 RNG를 반환할 수 있는 Rand를 반환한다.
   * @see Rand
   */
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIntDoubleViaBoth: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleIntViaBoth: Rand[(Double, Int)] =
    both(double, int)

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  // 6.2
  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count == 0 then (List.empty, rng)
    else
      val (h, r1) = rng.nextInt
      val (t, r2) = ints(count - 1)(r1)
      (h :: t, r2)

  // 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng =>
      val (a, r1) = f(rng)
      g(a)(r1)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)
    }

  // 6.9
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

type State[S, +A] = S => (A, S)

extension[S, A] (run: State[S, A])
  def flatMap[B](f: A => State[S, B]): State[S, B] = s =>
    val (a, next) = run(s)
    f(a)(next)

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](rs: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rs.map(b => f(a, b)))


object State:
  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def get[S]: State[S, S] = s => (s, s)
  def set[S](s: S): State[S, Unit] = _ => ((), s)
  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty))((f, acc) => f.map2(acc)(_ :: _))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
