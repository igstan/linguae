package codecamp
package test

import org.scalatest.{ FunSuite, Inside, Matchers }

abstract class Test extends FunSuite with Matchers with Inside
