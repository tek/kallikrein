package klk

import java.lang.reflect.Constructor

import cats.implicits._
import sbt.testing.{
  Event,
  EventHandler,
  Fingerprint,
  Logger,
  OptionalThrowable,
  Selector,
  Status,
  SubclassFingerprint,
  Task,
  TaskDef,
  TestSelector
}

case class TestThunk(desc: String, thunk: TestLog => KlkResult)

case class FinishEvent(
  status: Status,
  duration: Long,
  fingerprint: Fingerprint,
  fullyQualifiedName: String,
  selector: Selector,
  throwable: OptionalThrowable,
)
extends Event

object FinishEvent
{
  def cons(taskDef: TaskDef, name: String, status: Status, duration: Long): FinishEvent =
    FinishEvent(
      status,
      duration,
      taskDef.fingerprint,
      taskDef.fullyQualifiedName,
      new TestSelector(name),
      new OptionalThrowable,
    )
}

object ExecuteTask
{
  def apply[F[_]]
  (taskDef: TaskDef)
  (test: TestThunk)
  : (EventHandler, Array[Logger]) => Array[Task] =
    (events, log) => {
      val startTime = System.currentTimeMillis
      val success = KlkResult.successful(test.thunk(TestLog(log)))
      val status = if (success) Status.Success else Status.Failure
      val duration = System.currentTimeMillis - startTime
      events.handle(FinishEvent.cons(taskDef, test.desc, status, duration))
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
    test.tests.tests.toList.map(a => KlkTask(taskDef, ExecuteTask(taskDef)(a), Array.empty)).toArray

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
