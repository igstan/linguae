package ro.igstan.debugger

package object eval {
  type Result = Either[String, Value]
  type Kont = Resumption.Step => Resumption.Step
}
