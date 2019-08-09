package klk

import sbt.testing.{Framework, Fingerprint, SubclassFingerprint, Runner, Task, TaskDef}

object KlkFingerprint
extends SubclassFingerprint
{
  def superclassName: String =
    "klk.Test"

  def isModule: Boolean =
    true

  def requireNoArgConstructor: Boolean =
    false
}

class KlkFramework
extends Framework
{
  def name: String =
    "kallikrein"

  def fingerprints: Array[Fingerprint] =
    Array(KlkFingerprint)

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner =
    new Runner {
      println(1)
      def args: Array[String] =
        Array()

      def done: String =
        ""

      def remoteArgs: Array[String] =
        Array()

      def tasks(x: Array[TaskDef]): Array[Task] =
        Array()
    }
}

object Run
{
  
}
