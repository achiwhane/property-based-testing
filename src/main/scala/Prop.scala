import Prop.{FailedCase, SuccessCount}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  // has a side effect of running the tests and outputting report to
  // stdout
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}
