package klk

import java.lang.reflect.Constructor

import cats.implicits._
import sbt.testing.{EventHandler, Fingerprint, Logger, SubclassFingerprint, Task, TaskDef}

case class TestThunk(thunk: TestLog => KlkResult)

object ExecuteTask
{
  def apply[F[_]]
  (test: TestThunk)
  : (EventHandler, Array[Logger]) => Array[Task] =
    (_, log) => {
      test.thunk(TestLog(log))
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
  def testInstance(ctor: Constructor[_]): TestInterface = {
    ctor.setAccessible(true)
    ctor
      .asInstanceOf[Constructor[TestInterface]]
      .newInstance()
  }

  def fromTest(taskDef: TaskDef)(test: TestInterface): Array[KlkTask] =
    test.tests.tests.toList.map(a => KlkTask(taskDef, ExecuteTask(a), Array.empty)).toArray

  def classNameSuffix: Fingerprint => String = {
    case a: SubclassFingerprint if a.isModule => "$"
    case _ => ""
  }

  def className(taskDef: TaskDef): String =
    taskDef.fullyQualifiedName + classNameSuffix(taskDef.fingerprint)

  def fromTaskDef(loader: ClassLoader)(taskDef: TaskDef): Array[KlkTask] =
    loader
      .loadClass(className(taskDef))
      .asInstanceOf[Class[TestInterface]]
      .getDeclaredConstructors
      .headOption
      .map(testInstance)
      .map(fromTest(taskDef))
      .getOrElse(Array.empty)
}

object KlkTasks
{
  def process(testClassLoader: ClassLoader)(taskDefs: Array[TaskDef]): Array[Task] =
    taskDefs.flatMap(KlkTask.fromTaskDef(testClassLoader))
}
