import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .grouped(3).zipWithIndex
  .foreach { case (Seq(c, _, prices), t) =>
    val items = new Array[Int](1000)
    val credit = c.toInt
    def loop(): Unit = prices
      .split(' ').view
      .map(_.toInt).zipWithIndex
      .foreach { case (price, i) =>
        val i1 = i + 1
        val p = price - 1
        val r = credit - price - 1
        if (r >= 0 && items(r) > 0) {
          println(s"Case #${t + 1}: ${items(r)} $i1")
          return
        } else if (items(p) <= 0) {
          items(p) = i1
        }
      }

    loop()
  }
