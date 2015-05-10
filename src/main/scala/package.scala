package ro.igstan

package object debugger {
  def ignore[A](a: A): Unit = ()

  implicit class Ignore[A](a: A) {
    def ignore(): Unit = ()
  }

  import org.scalajs.dom.ext.EasySeq
  import org.scalajs.dom

  implicit class RichDOMList[A](coll: org.scalajs.dom.DOMList[A]) extends EasySeq[A](coll.length, coll.apply)
}
