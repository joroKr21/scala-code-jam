import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .map(_.toInt).zipWithIndex
  .foreach { case (step, t) =>
    val t1 = t + 1
    if (step == 0) {
      println(s"Case #$t1: INSOMNIA")
    } else {
      var sheep = step
      var digits = 0
      while (digits != 0x3ff) {
        var current = sheep
        while (current != 0) {
          digits |= 1 << (current % 10)
          current /= 10
        }

        sheep += step
      }

      sheep -= step
      println(s"Case #$t1: $sheep")
    }
  }
