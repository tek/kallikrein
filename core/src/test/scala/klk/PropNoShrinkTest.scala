package klk

import cats.data.NonEmptyList
import cats.effect.IO

class PropNoShrinkTest
extends KlkSpecification[IO]
{
  val target: KlkResult[Unit] =
    KlkResult.Single((), true, KlkResult.Details.NoDetails)

  "property test, no shrink" >> {
    val result = test(_.forallNoShrink((l: List[Int]) => IO(l.size < 5)))
    result match {
      case KlkResult.Single((), success, KlkResult.Details.Simple(NonEmptyList(head, _))) =>
        success.must(beFalse).and(head.must(startWith("failed after")))
      case _ => false.must(beTrue)
    }
  }
}
