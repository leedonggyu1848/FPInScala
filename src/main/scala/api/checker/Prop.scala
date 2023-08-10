package api.checker

import api.checker.Prop.{FailedCase, SuccessCount}
import rng.{RNG, SimpleRNG, State}

object Prop :
  type FailedCase = String
  type SuccessCount = Int
end Prop


trait Prop :
  def check: Either[(FailedCase, FailedCase), SuccessCount]
  def &&(p: Prop): Prop
end Prop
