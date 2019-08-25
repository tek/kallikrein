package klk

import sbt.testing.{Fingerprint, Framework, Runner, SubclassFingerprint, Task, TaskDef}

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
