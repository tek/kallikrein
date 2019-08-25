package klk

import java.lang.reflect.Constructor

import cats.implicits._
import sbt.testing.{EventHandler, Logger, Task, TaskDef}

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
