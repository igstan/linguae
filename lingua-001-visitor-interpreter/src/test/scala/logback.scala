package lingua001
package test

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.pattern.color.ANSIConstants._
import ch.qos.logback.core.pattern.color.ForegroundCompositeConverterBase

class LogLevelHighlighting extends ForegroundCompositeConverterBase[ILoggingEvent] {
  override def getForegroundColorCode(event: ILoggingEvent): String = {
    (event.getLevel.toInt: @annotation.switch) match {
      case Level.ERROR_INT => RED_FG
      case Level.WARN_INT  => YELLOW_FG
      case _               => DEFAULT_FG
    }
  }
}
