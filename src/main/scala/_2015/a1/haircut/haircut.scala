import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .map(_.split(' ').map(_.toInt).view)
  .grouped(2).zipWithIndex
  .foreach { case (Seq(Seq(_, customers), barbers), t) =>
    def barber: Int = {
      val rate = barbers.map(1.0 / _).sum
      var time = ((0 max (customers - barbers.size - 1)) / rate).toLong - 1
      val served = if (time < 0) 0 else barbers.map(time / _ + 1).sum
      var remaining = customers - served
      while (remaining > 0) {
        time += barbers.map(b => b - time % b).min
        for ((b, i) <- barbers.zipWithIndex if time % b == 0) {
          remaining -= 1
          if (remaining == 0) return i + 1
        }
      }

      0
    }

    println(s"Case #${t + 1}: $barber")
  }
