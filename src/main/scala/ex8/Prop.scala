package ex8

import ex8.Prop.*

object Prop:
  type FailedCase = String
  type SuccessCount = Int
  
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = ???

trait Prop:
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  // 8.3
  def &&(p: Prop): Prop = new Prop:
    def check = {
      Prop.this.check
      p.check
    }



