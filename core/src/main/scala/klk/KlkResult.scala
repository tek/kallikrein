package klk

sealed trait KlkResultDetails[A, B]

object KlkResultDetails
{
  case class NoDetails[A, B]()
  extends KlkResultDetails[A, B]

  case class Simple[A, B](info: List[String])
  extends KlkResultDetails[A, B]

  case class Complex[A, B](desc: List[String], target: A, actual: B)
  extends KlkResultDetails[A, B]

  case class Fatal[A, B](error: Throwable)
  extends KlkResultDetails[A, B]
}

case class KlkResult[A, B](success: Boolean, details: KlkResultDetails[A, B])

object KlkResult
{
  def bool(success: Boolean): KlkResult[Boolean, Boolean] =
    KlkResult(success, KlkResultDetails.NoDetails())
}
