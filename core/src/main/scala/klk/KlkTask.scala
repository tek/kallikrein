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
  (test: TestThunk[SbtResources])
  : (EventHandler, Array[Logger]) => Array[Task] =
    (events, log) => {
      val startTime = System.currentTimeMillis
      val success = KlkResult.successful(test.thunk(SbtResources(SbtTestLog(log))))
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
  case class Error(details: Error.Details, exception: Option[Throwable])

  object Error
  {
    sealed trait Details

    object Details
    {
      case class LoadClass(name: String)
      extends Details

      case class CastClass(name: String, cls: Class[_])
      extends Details

      case class Ctors(cls: Class[_])
      extends Details

      case class NoCtor(cls: Class[_])
      extends Details

      case class CastCtor(ctor: Constructor[_])
      extends Details

      case class SetAccessible(ctor: Constructor[_])
      extends Details

      case class Instantiate(ctor: Constructor[_])
      extends Details
    }
  }

  def fromTest(taskDef: TaskDef)(test: TestInterface[SbtResources]): List[KlkTask] =
    test.tests.tests.map(a => KlkTask(taskDef, ExecuteTask(taskDef)(a), Array.empty))

  def classNameSuffix: Fingerprint => String = {
    case a: SubclassFingerprint if a.isModule => "$"
    case _ => ""
  }

  def className(taskDef: TaskDef): String =
    taskDef.fullyQualifiedName + classNameSuffix(taskDef.fingerprint)

  def safe[A](error: Error.Details)(f: => A): Either[Error, A] =
    Either.catchOnly[Throwable](f).leftMap(e => Error(error, Some(e)))

  def loadClass(loader: ClassLoader)(name: String): Either[Error, Class[TestInterface[SbtResources]]] =
    for {
      cls <- safe(Error.Details.LoadClass(name))(loader.loadClass(name))
      cast <- safe(Error.Details.CastClass(name, cls))(cls.asInstanceOf[Class[TestInterface[SbtResources]]])
    } yield cast

  def findCtor(cls: Class[TestInterface[SbtResources]]): Either[Error, Constructor[TestInterface[SbtResources]]] =
    for {
      ctors <- safe(Error.Details.Ctors(cls))(cls.getDeclaredConstructors)
      ctor <- Either.fromOption(ctors.headOption, Error(Error.Details.NoCtor(cls), None))
      _ <- safe(Error.Details.SetAccessible(ctor))(ctor.setAccessible(true))
      cast <- safe(Error.Details.CastCtor(ctor))(ctor.asInstanceOf[Constructor[TestInterface[SbtResources]]])
    } yield cast

  def fromTaskDef(loader: ClassLoader)(taskDef: TaskDef): Either[Error, List[KlkTask]] =
    for {
      cls <- loadClass(loader)(className(taskDef))
      ctor <- findCtor(cls)
      inst <- safe(Error.Details.Instantiate(ctor))(ctor.newInstance())
    } yield fromTest(taskDef)(inst)

}

object KlkTasks
{
  def error: KlkTask.Error.Details => String = {
    case KlkTask.Error.Details.LoadClass(name) =>
      s"could not load class $name:"
    case KlkTask.Error.Details.CastClass(name, cls) =>
      s"could not cast class $name ($cls) to TestInterface:"
    case KlkTask.Error.Details.Ctors(cls) =>
      s"error when getting ctors for $cls:"
    case KlkTask.Error.Details.NoCtor(cls) =>
      s"class $cls has no ctors"
    case KlkTask.Error.Details.CastCtor(ctor) =>
      s"could not cast ctor $ctor:"
    case KlkTask.Error.Details.SetAccessible(ctor) =>
      s"could not make ctor $ctor accessible:"
    case KlkTask.Error.Details.Instantiate(ctor) =>
      s"could not instantiate constructor $ctor:"
  }

  def logError(loggers: Array[Logger])(lines: List[String]): Array[Task] = {
    SbtTestLog.unsafe(SbtTestLog(loggers))(_.error)(lines)
    Array.empty
  }

  def taskImpl: KlkTask.Error => (EventHandler, Array[Logger]) => Array[Task] = {
    case KlkTask.Error(details, exception) =>
      (_, loggers) =>
        logError(loggers)(error(details) :: exception.toList.map(_.getMessage))
  }

  def errorTask(taskDef: TaskDef)(error: KlkTask.Error): List[KlkTask] =
    List(KlkTask(taskDef, taskImpl(error), Array.empty))

  def processTaskDef(testClassLoader: ClassLoader)(taskDef: TaskDef): List[KlkTask] =
    KlkTask.fromTaskDef(testClassLoader)(taskDef).valueOr(errorTask(taskDef))

  def process(testClassLoader: ClassLoader)(taskDefs: Array[TaskDef]): Array[Task] =
    taskDefs.toList.flatMap(processTaskDef(testClassLoader)).toArray
}
