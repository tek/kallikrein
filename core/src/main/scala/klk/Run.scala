package klk

import cats.Eq
import cats.implicits._
import cats.effect.IO
import sbt.testing.{Framework, Fingerprint, SubclassFingerprint, Runner, Task, TaskDef, EventHandler, Logger}
import scala.collection.mutable

sealed trait TestResultDetails[+A, +B]

object TestResultDetails
{
  case object NoDetails
  extends TestResultDetails[Nothing, Nothing]

  case class Simple(info: List[String])
  extends TestResultDetails[Nothing, Nothing]

  case class Complex[A, B](desc: List[String], target: A, actual: B)
  extends TestResultDetails[A, B]
}

case class TestResult[A, B](success: Boolean, details: TestResultDetails[A, B])

trait TestReporter[A, B]
{
  def failure: TestResult[A, B] => Unit
}

object TestReporter
{
  def format[A, B]: TestResultDetails[A, B] => List[String] = {
    case TestResultDetails.NoDetails =>
      List("test failed")
    case TestResultDetails.Simple(info) =>
      info
    case TestResultDetails.Complex(desc, target, actual) =>
      desc ::: List(target.toString, actual.toString)
  }

  def stdout[A, B]: TestReporter[A, B] =
    new TestReporter[A, B] {
      def failure: TestResult[A, B] => Unit =
        result => format(result.details).foreach(println)
    }
}

trait TestRunner[F[_]]
{
  def run[A, B](thunk: F[TestResult[A, B]]): TestResult[A, B]
}

object TestRunner
{
  def io: TestRunner[IO] =
    new TestRunner[IO] {
      def run[A, B](thunk: IO[TestResult[A, B]]): TestResult[A, B] =
        thunk.attempt.unsafeRunSync.valueOr(a => TestResult(false, TestResultDetails.Simple(List(a.toString))))
    }
}

case class KlkTest[F[_], A, B](desc: String, thunk: F[TestResult[A, B]], runner: TestRunner[F])

object KlkTest
{
  def run[F[_], A, B]: KlkTest[F, A, B] => TestResult[A, B] = {
    case KlkTest(_, thunk, runner) =>
      runner.run(thunk)
  }
}

trait ConstructTest[F[_], T, A, B]
{
  def apply(desc: String)(thunk: T): KlkTest[F, A, B]
}

object ConstructTest
{
  implicit def ConstructTest_IO_Boolean: ConstructTest[IO, IO[Boolean], Boolean, Boolean] =
    new ConstructTest[IO, IO[Boolean], Boolean, Boolean] {
      def apply(desc: String)(thunk: IO[Boolean]): KlkTest[IO, Boolean, Boolean] =
        KlkTest(desc, thunk.map(TestResult(_, TestResultDetails.NoDetails)), TestRunner.io)
    }

  implicit def ConstructTest_IO_TestResult[A, B]: ConstructTest[IO, IO[TestResult[A, B]], A, B] =
    new ConstructTest[IO, IO[TestResult[A, B]], A, B] {
      def apply(desc: String)(thunk: IO[TestResult[A, B]]): KlkTest[IO, A, B] =
        KlkTest(desc, thunk, TestRunner.io)
    }
}

trait Test
{
  def reporter[A, B]: TestReporter[A, B]

  val tests: mutable.Buffer[KlkTest[F, A, B] forSome { type F[_]; type A; type B }] =
    mutable.Buffer.empty

  def test[F[_], T, A, B](desc: String)(thunk: T)(implicit ct: ConstructTest[F, T, A, B]): Unit = {
    tests += ct(desc)(thunk)
  }

  import scala.language.implicitConversions

  implicit def test0[T, A, B](desc: String)(t: => T)(implicit ct: ConstructTest[IO, () => T, A, B]): Unit =
    test(desc)(() => t)
}

trait SimpleAssertions
{
  def assert(desc: String)(value: Boolean): TestResult[Boolean, Boolean] =
    TestResult(value, TestResultDetails.Simple(List(desc)))

  def assertEqual[A](target: A)(candidate: A)(implicit eql: Eq[A]): TestResult[A, A] =
    TestResult(eql.eqv(target, candidate), TestResultDetails.Complex(List("values are not equal"), target, candidate))
}

trait SimpleTest
extends Test
with SimpleAssertions
{
  def reporter[A, B]: TestReporter[A, B] =
    TestReporter.stdout
}

object KlkFingerprint
extends SubclassFingerprint
{
  def superclassName: String =
    "klk.Test"

  def isModule: Boolean =
    true

  def requireNoArgConstructor: Boolean =
    true
}

case class KlkTask(taskDef: TaskDef, exe: (EventHandler, Array[Logger]) => Array[Task], tags: Array[String])
extends Task
{
  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
    exe(handler, loggers)
}

case class PresentTask(taskDef: TaskDef, exe: (EventHandler, Array[Logger]) => Array[Task], tags: Array[String])
extends Task
{
  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
    exe(handler, loggers)
}

object PresentTask
{
  def present[A, B](result: TestResult[A, B]): (EventHandler, Array[Logger]) => Array[Task] =
    (_, _) => {
      println(result)
      Array()
    }

  def forResult[A, B](taskDef: TaskDef)(result: TestResult[A, B]): Task =
    PresentTask(taskDef, present(result), Array.empty)

}

object ExecuteTask
{
  def apply[F[_], A, B](taskDef: TaskDef, test: KlkTest[F, A, B]): (EventHandler, Array[Logger]) => Array[Task] = {
    (_, _) => Array(PresentTask.forResult(taskDef)(KlkTest.run(test)))
  }
}

object KlkTask
{
  def fromTaskDef(loader: ClassLoader)(taskDef: TaskDef): Array[KlkTask] = {
    val cls = loader.loadClass(taskDef.fullyQualifiedName + "$").asInstanceOf[Class[Test]]
    val ctor = cls.getDeclaredConstructors.head
    ctor.setAccessible(true)
    ctor.newInstance().asInstanceOf[Test].tests.toArray.map {
      case a =>
        KlkTask(taskDef, ExecuteTask(taskDef, a), Array.empty)
    }
  }
}

class KlkFramework
extends Framework
{
  def name: String =
    "kallikrein"

  def fingerprints: Array[Fingerprint] =
    Array(KlkFingerprint)

  def runner(args0: Array[String], remoteArgs0: Array[String], testClassLoader: ClassLoader): Runner =
    new Runner {
      def args: Array[String] =
        args0

      def done: String =
        "done!"

      def remoteArgs: Array[String] =
        remoteArgs0

      def tasks(x: Array[TaskDef]): Array[Task] = {
        x.flatMap(KlkTask.fromTaskDef(testClassLoader))
      }
    }
}
