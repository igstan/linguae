package ro.igstan

import scala.annotation.unused

package object debugger {
  def ignore[A](@unused a: A): Unit = ()

  implicit class Ignore[A](a: A) {
    def ignore(): Unit = ()
  }
}
