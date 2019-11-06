package klk

import cats.data.NonEmptyList
import cats.effect.IO

// TODO specify seed and test the value
class PropShrinkTest
extends KlkSpecification[IO]
{
  val target: KlkResult[Unit] =
    KlkResult.Single((), true, KlkResult.Details.NoDetails)

  "property test, shrink" >> {
    val result = test(_.forall((i: Int) => IO.pure(i > 0)))
    result match {
      case KlkResult.Single((), success, KlkResult.Details.Simple(NonEmptyList(head, _))) =>
        success.must(beFalse).and(head.must(startWith("failed after")))
      case a => a.must_==("wrong result")
    }
  }
}
