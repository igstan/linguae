package ro.igstan

package object debugger {
  def ignore[A](a: A): Unit = ()

  implicit class Ignore[A](a: A) {
    def ignore(): Unit = ()
  }
}
