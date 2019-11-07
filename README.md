# Intro

**kallikrein** is a Scala testing framework for sbt focused on running [cats-effect] based programs.

If you're into matcher DSLs, check out [xpct], which is a framework-agnostic typed matcher lib with support for
**kallikrein**.

## module id

```sbt
"io.tryp" %% "kallikrein-sbt" % "0.3.0"
```

## sbt

To use the framework in a project, specify the setting:

```sbt
testFrameworks += new TestFramework("klk.KlkFramework")
```

# Basics

## Imperative DSL

```scala
class SomeTest
extends klk.IOTest
{
  test("description")(IO.pure(1 == 1))
}
```

Tests are added by calling the `test` function in a class inheriting `klk.SimpleTest[F]`, where `F[_]` is an effect that
implements `cats.effect.Sync`.
`klk.IOTest` is a convenience trait using `cats.effect.IO`.

The effect type of an individual test can be different from the main effect if there is an instance of `klk.Compile[F,
G]`.
For example, `EitherT` is supported out of the box:

```scala
test("EitherT")(EitherT.right[Unit](IO.pure(1 == 1)))
```

A `Left` value will be converted into a failure.

Assertions are returned from the test thunk and can be anything, as long as there is an instance for `klk.TestResult`.
The internal type representing the result is `KlkResult`.

## Composable Tests

The above mentioned `test` builder can also be used in a pure context and has a nice arsenal of typeclass instances for
composition.

When tests are sequenced in a for comprehension, the semantic effect is that of conditional execution:
If one test fails, all following tests are skipped.

There is an instance of `SemigroupK` available, allowing you to use the `<+>` operator, resulting in the alternative, or
`unless`, semantics – i.e. if and only if the first test fails, execute the second one and use its result.

For independent tests, there are two combinators: `sequential` and `parallel`.
They do what you would expect, similar to the imperative test building syntax.
The `parallel` variant requires an instances of `cats.Parallel` and will execute the tests with a `parTraverse`.

The following example will result in a successful end result:

```scala
class DepTest
extends ComposeTest[IO, SbtResources]
{
  def testSuccess: IO[Boolean] =
    IO.pure(true)

  def testFail: IO[Boolean] =
    IO.raiseError(new Exception("boom"))

  implicit def cs: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  def tests: Suite[IO, Unit, Unit] =
    for {
      _ <- sharedResource(Resource.pure(5))(
        builder =>
          builder.test("five is 4")(five => IO.pure(five == 4)) <+>
          builder.test("five is 5")(five => IO.pure(five == 5))
      )
      _ <- test("test 1")(testSuccess)
      _ <- test("test 2")(testFail) <+> test("test 3")(testSuccess)
      _ <- Suite.parallel(test("test 4a")(testSuccess), test("test 4b")(testSuccess)) <+> test("test 5")(testFail)
      _ <- test("test 7")(testSuccess)
    } yield ()
}
```

# Resources

Tests can depend on shared and individual resources that will be supplied by the framework when running:

```scala
class SomeTest
extends klk.IOTest
{
  val res1: Resource[IO, Int] = Resource.pure(1)

  val res2: Resource[IO, Int] = Resource.pure(1)

  test("resource").resource(res1).resource(res2)((i: Int) => (j: Int) => IO.pure(i == j))

  def eightySix: SharedResource[IO, Int] =
    sharedResource(Resource.pure(86))

  eightySix.test("shared resource 1")(i => IO.pure(i == 86))

  eightySix.test("shared resource 2")(i => IO.pure(i == 68))
}
```

The resource parameters in the builder are typed, so the compiler will complain if they don't match.

The shared resource will be acquired only once and supplied to all tests that use it.

# Property Testing

[Scalacheck] can be used in a test by calling the `forall` method on a test builder:

```scala
class SomeTest
extends klk.IOTest
{
  test("are all lists of integers shorter than 5 elements?").forall((l1: List[Int]) => IO(l1.size < 5))
}
```

This features a custom runner for the properties built on fs2.

A second variant `forallNoShrink` does what it advertises.

# Law Checking

[cats-discipline] laws can be checked like this:

```scala
class SomeTest
extends klk.IOTest
{
  test("laws").laws(IO.pure(FunctorTests[List].functor[Int, Int, Int]))
}
```

[cats-effect]: https://github.com/typelevel/cats-effect
[xpct]: https://github.com/tek/xpct
[scalacheck]: https://github.com/typelevel/scalacheck
[cats-discipline]: https://github.com/typelevel/discipline
