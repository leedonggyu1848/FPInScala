package ex8

import ex6.RNG
import Prop.*
import ex8.Gen.*

import scala.annotation.targetName

object Prop:
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result:
    def isFalsified: Boolean
  case object Passed extends Result:
    def isFalsified = false

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result:
    def isFalsified = true

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(LazyList.from(0)).take(n).map {
        case (a, i) => try
          if f(a) then Passed else Falsified(a.toString, i)
        catch
          case e: Exception => Falsified(buildMsg(a, e), i)
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"



  case class Prop(run: (TestCases, RNG) => Result):
    def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

    // 8.9
    @targetName("and") def &&(p: Prop): Prop = ???
    @targetName("or") def ||(p: Prop): Prop = ???

