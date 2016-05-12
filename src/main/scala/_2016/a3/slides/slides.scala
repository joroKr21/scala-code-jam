import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .map(_ split ' ').zipWithIndex
  .foreach { case (Array(b, w), t) =>
    val buildings = b.toInt
    val ways = w.toLong
    val maxWays = 1l << buildings - 2
    print(s"Case #${t + 1}: ")
    if (ways > maxWays) {
      println("IMPOSSIBLE")
    } else {
      println("POSSIBLE")
      val first = padLeftTo(buildings, '0') {
        if (ways == maxWays) fill(buildings - 1)('1')
        else (ways << 1).toBinaryString
      }
      val between = for (b <- 2 until buildings) yield
        fill(b)('0').padTo(buildings, '1')
      val last = fill(buildings)('0')
      (first +: between :+ last) foreach println
    }
  }

def fill(n: Int)(ch: Char): String =
  new String(Array.fill(n)(ch))

def padLeftTo(n: Int, ch: Char)(str: String): String =
  fill(n - str.length)(ch) + str
