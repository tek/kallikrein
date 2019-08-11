package klk

import java.lang.reflect.Constructor

import scala.collection.mutable
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext

import cats.Eq
import cats.implicits._
import cats.effect.{IO, Timer}
import sbt.testing.{Framework, Fingerprint, SubclassFingerprint, Runner, Task, TaskDef, EventHandler, Logger}

import StringColor._
import StringColors.color

case class TestLog(loggers: Array[Logger])
{
  def info(lines: List[String]): Unit =
    loggers.foreach(l => lines.foreach(l.info))
}

sealed trait TestResultDetails[A, B]

object TestResultDetails
{
  case class NoDetails[A, B]()
  extends TestResultDetails[A, B]

  case class Simple[A, B](info: List[String])
  extends TestResultDetails[A, B]

  case class Complex[A, B](desc: List[String], target: A, actual: B)
  extends TestResultDetails[A, B]

  case class Fatal[A, B](error: Throwable)
  extends TestResultDetails[A, B]
}

case class TestResult[A, B](success: Boolean, details: TestResultDetails[A, B])

trait TestReporter[A, B]
{
  def result(log: TestLog): String => Boolean => Unit
  def failure(log: TestLog): TestResultDetails[A, B] => Unit
}

object TestReporter
{
  def indent(spaces: Int)(lines: List[String]): List[String] =
    lines.map(a => s"${" " * spaces}$a")

  def sanitizeStacktrace(trace: List[StackTraceElement]): List[String] =
    trace
      .takeWhile(a => !a.getClassName.startsWith("klk.TestRunner"))
      .reverse
      .dropWhile(a => a.getClassName.startsWith("cats.effect"))
      .dropWhile(a => a.getClassName.startsWith("scala.runtime"))
      .reverse
      .map(_.toString)

  def formatFailure[A, B]: TestResultDetails[A, B] => List[String] = {
    case TestResultDetails.NoDetails() =>
      List("test failed")
    case TestResultDetails.Simple(info) =>
      info
    case TestResultDetails.Complex(desc, target, actual) =>
      desc ::: indent(2)(List(s"target: ${target.toString.green}", s"actual: ${actual.toString.magenta}"))
    case TestResultDetails.Fatal(error) =>
      s"${"test threw".blue} ${error.toString.magenta}" :: indent(2)(sanitizeStacktrace(error.getStackTrace.toList))
  }

  def successSymbol: Boolean => String = {
    case false => "✘".red
    case true => "✔".green
  }

  def formatResult(desc: String)(success: Boolean): List[String] =
    List(s"${successSymbol(success)} $desc")

  def stdout[A, B]: TestReporter[A, B] =
    new TestReporter[A, B] {
      def result(log: TestLog): String => Boolean => Unit =
        desc => (log.info _).compose(formatResult(desc))

      def failure(log: TestLog): TestResultDetails[A, B] => Unit =
        (log.info _).compose(indent(2)).compose(formatFailure)
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
        thunk
          .recover { case NonFatal(a) => TestResult(false, TestResultDetails.Fatal[A, B](a)) }
          .unsafeRunSync
    }
}

case class KlkTest[F[_], A, B](desc: String, thunk: F[TestResult[A, B]], runner: TestRunner[F])

object KlkTest
{
  def logResult[A, B](reporter: TestReporter[A, B], log: TestLog)(desc: String, result: TestResult[A, B]): Unit = {
    reporter.result(log)(desc)(result.success)
    if (!result.success) reporter.failure(log)(result.details)
  }

  def run[F[_], A, B](reporter: TestReporter[A, B], log: TestLog): KlkTest[F, A, B] => TestResult[A, B] = {
    case KlkTest(desc, thunk, runner) =>
      val result = runner.run(thunk)
      logResult(reporter, log)(desc, result)
      result
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
        KlkTest(desc, thunk.map(TestResult(_, TestResultDetails.NoDetails())), TestRunner.io)
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

  def test[F[_], T, A, B](desc: String)(thunk: T)(implicit ct: ConstructTest[F, T, A, B]): Unit =
    tests += ct(desc)(thunk)

  import scala.language.implicitConversions

  implicit def test0[T, A, B](t: => T)(implicit ct: ConstructTest[IO, () => T, A, B]): () => T =
    () => ct match { case _ => t }
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

  implicit def timer: Timer[IO] =
    IO.timer(ExecutionContext.global)
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

object ExecuteTask
{
  def apply[F[_], A, B]
  (reporter: TestReporter[A, B])
  (test: KlkTest[F, A, B])
  : (EventHandler, Array[Logger]) => Array[Task] =
    (_, log) => {
      KlkTest.run(reporter, TestLog(log))(test)
      Array()
    }
}

case class KlkTask(taskDef: TaskDef, exe: (EventHandler, Array[Logger]) => Array[Task], tags: Array[String])
extends Task
{
  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
    exe(handler, loggers)
}

object KlkTask
{
  def testInstance(ctor: Constructor[_]): Test = {
    ctor.setAccessible(true)
    ctor
      .newInstance()
      .asInstanceOf[Test]
  }

  def fromTest(taskDef: TaskDef)(test: Test): Array[KlkTask] =
    test
      .tests
      .toArray
      .map(a => KlkTask(taskDef, ExecuteTask(test.reporter)(a), Array.empty))

  def fromTaskDef(loader: ClassLoader)(taskDef: TaskDef): Array[KlkTask] =
    loader
      .loadClass(taskDef.fullyQualifiedName + "$")
      .asInstanceOf[Class[Test]]
      .getDeclaredConstructors
      .headOption
      .map(testInstance)
      .map(fromTest(taskDef))
      .getOrElse(Array.empty)
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

      def tasks(x: Array[TaskDef]): Array[Task] =
        x.flatMap(KlkTask.fromTaskDef(testClassLoader))
    }
}
