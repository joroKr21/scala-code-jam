import scala.math._
import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null).zipWithIndex
  .foreach { case (input, t) =>
    println(s"Case #${t + 1}: $input")
  }
