package ro.igstan

import scala.annotation.unused
import org.scalajs.dom
import dom.DOMList
import dom.ext.EasySeq

package object debugger {
  def ignore[A](@unused a: A): Unit = ()

  implicit class Ignore[A](a: A) {
    def ignore(): Unit = ()
  }

  implicit class RichDOMList[A](list: DOMList[A]) extends EasySeq[A](list.length, list.apply)
}
