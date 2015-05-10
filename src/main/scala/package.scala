package ro.igstan

import org.scalajs.dom, dom.DOMList, dom.ext.EasySeq

package object debugger {
  def ignore[A](a: A): Unit = ()

  implicit class Ignore[A](a: A) {
    def ignore(): Unit = ()
  }

  implicit class RichDOMList[A](list: DOMList[A]) extends EasySeq[A](list.length, list.apply)
}
