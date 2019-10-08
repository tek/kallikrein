# Intro

**kallikrein** is a Scala testing framework for sbt focused on running cats-effect based programs.

## module id

```sbt
"io.tryp" %% "kallikrein" % "0.0.0"
```

# Basics

```scala
class SomeTest
extends klk.IOTest
{
  test("description")(IO.pure(1 == 1))
}
```

Tests are added by calling the `test` function in a class inheriting `klk.SimpleTest[F]`, where `F[_]` is an effect that
implements `cats.effect.Sync`.

Assertions are returned from the test thunk and can be anything, as long as there is an instance for `klk.TestResult`.
The internal type representing the result is `KlkResult[A, B]`, where the parameters are the types of the expectation
and result, which are both `Boolean` in the example.

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

Scalacheck can be used in a test by calling the `forall` method on a test builder:

```scala
class SomeTest
extends klk.IOTest
{
  test("are all lists of integers shorter than 5 elements?").forall((l1: List[Int]) => IO(l1.size < 5))
}
```

This features a custom runner for the properties built on fs2.
