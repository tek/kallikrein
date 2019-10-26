package klk

import sbt.testing.{Fingerprint, Framework, Runner, SubclassFingerprint, Task, TaskDef}

object KlkClassFingerprint
extends SubclassFingerprint
{
  def superclassName: String =
    "klk.TestMarker"

  def isModule: Boolean =
    false

  def requireNoArgConstructor: Boolean =
    true
}

object KlkModuleFingerprint
extends SubclassFingerprint
{
  def superclassName: String =
    "klk.TestMarker"

  def isModule: Boolean =
    true

  def requireNoArgConstructor: Boolean =
    true
}

object KlkConfigFingerprint
extends SubclassFingerprint
{
  def superclassName: String =
    "klk.TestConfig"

  def isModule: Boolean =
    true

  def requireNoArgConstructor: Boolean =
    true
}

class KlkFramework
extends Framework
{
  def name: String =
    "kallikrein"

  def fingerprints: Array[Fingerprint] =
    Array(KlkClassFingerprint, KlkModuleFingerprint, KlkConfigFingerprint)

  def runner(args0: Array[String], remoteArgs0: Array[String], testClassLoader: ClassLoader): Runner =
    new Runner {
      def args: Array[String] =
        args0

      def done: String =
        ""

      def remoteArgs: Array[String] =
        remoteArgs0

      def tasks(taskDefs: Array[TaskDef]): Array[Task] =
        KlkTasks.process(testClassLoader)(taskDefs)
    }
}
