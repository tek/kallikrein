package klk

import scala.collection.mutable

import cats.effect.Resource

trait Test
{
  def reporter[A, B]: TestReporter[A, B]

  val tests: mutable.Buffer[KlkTest[F, A, B] forSome { type F[_]; type A; type B }] =
    mutable.Buffer.empty

  class TestPartiallyApplied[F[_]](desc: String)
  {
    def apply[Thunk, Params, Output, Expected, Actual]
    (thunk: Thunk)
    (
      implicit
      input: TestInput.Aux[F, Thunk, Output],
      result: TestResult[F, Output, Expected, Actual],
      effect: TestEffect[F],
    )
    : Unit =
      tests += Test[F, Thunk, Params, Output, Expected, Actual](input, result, effect)(desc)(thunk)

    def forall[Thunk, Params, Expected, Actual]
    (thunk: Thunk)
    (
      implicit
      input: PropGen[F, Thunk],
      result: TestResult[F, PropertyTestResult, Expected, Actual],
      effect: TestEffect[F],
    )
    : Unit =
      tests += Test.forall[F, Thunk, Params, PropertyTestResult, Expected, Actual](input, result, effect)(desc)(thunk)

    def resource[A](res: Resource[F, A]): Unit =
      ???
  }

  def test[F[_]]
  (desc: String)
  : TestPartiallyApplied[F] =
    new TestPartiallyApplied(desc)

  // import scala.language.implicitConversions

  // implicit def test0[T, A, B](t: => T)(implicit output: ConstructTest[IO, () => T, A, B]): () => T =
  //   () => output match { case _ => t }
}

object Test
{
  def apply[F[_], T, P, O, E, R]
  (input: TestInput.Aux[F, T, O], result: TestResult[F, O, E, R], effect: TestEffect[F])
  (desc: String)
  (thunk: T)
  : KlkTest[F, E, R] =
    KlkTest(desc, result.handle(TestFunction.execute(input.bracket(thunk))), effect)

  def forall[F[_], T, P, O, E, R]
  (input: PropGen[F, T], result: TestResult[F, PropertyTestResult, E, R], effect: TestEffect[F])
  (desc: String)
  (thunk: T)
  : KlkTest[F, E, R] =
    KlkTest(
      desc,
      result.handle(TestFunction.execute(PropGen(effect.concurrentPool)(thunk)(effect.sync, input))),
      effect,
    )
}
