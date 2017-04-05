import scala.Ordering.Implicits._
import scala.io.StdIn
import scala.util.control.Breaks._

object D {
  def unapply(c: Char): Option[Int] =
    if (c.isDigit) Some(c.asDigit) else None
}

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .map(_ split ' ').zipWithIndex
  .foreach { case (Array(coders, jammers), t) =>
    val n = coders.length
    var (exp, diff) = (1l, 0l)
    val memo @ Seq(cmin, cmid, cmax, jmin, jmid, jmax) =
      for (_ <- 1 to 6) yield Array.fill(n + 1)(0l)

    for (i <- n - 1 to 0 by -1) {
      def update(deltas: Int*) =
        for ((xs, d) <- memo zip deltas)
          xs(i) = xs(i + 1) + d * exp

      (coders(i), jammers(i)) match {
        case ('?', '?') =>
          update(0, 0, 9, 0, 0, 9)
        case ('?', D(j)) =>
          update(0, j, 9, j, j, j)
        case (D(c), '?') =>
          update(c, c, c, 0, c, 9)
        case (D(c), D(j)) =>
          update(c, c, c, j, j, j)
          diff += (c - j) * exp
      }

      exp *= 10
    }

    var score = (diff.abs, cmid.head, jmid.head)
    if (diff != 0) breakable {
      for (i <- 0 until n; k = i + 1) {
        exp /= 10
        val csofar = cmid.head - cmid(i)
        val jsofar = jmid.head - jmid(i)
        def update(deltas: Int*) = {
          val Seq(c1, j1, c2, j2, d1, d2) = deltas
          val diff1 = (d1 * exp + jmin(k) - cmax(k)).abs
          val diff2 = (d2 * exp + cmin(k) - jmax(k)).abs
          val cs1 = csofar + c1 * exp + cmax(k)
          val cs2 = csofar + c2 * exp + cmin(k)
          val js1 = jsofar + j1 * exp + jmin(k)
          val js2 = jsofar + j2 * exp + jmax(k)
          score = score min (diff1, cs1, js1) min (diff2, cs2, js2)
        }

        (coders(i), jammers(i)) match {
          case ( '?',  '?') => update(0, 1, 1, 0, 1, 1)
          case ( '?',  '0') => update(0, 0, 1, 0, 0, 1)
          case ( '?',  '9') => update(8, 9, 9, 9, 1, 0)
          case ( '0',  '?') => update(0, 1, 0, 0, 1, 0)
          case ( '9',  '?') => update(9, 9, 9, 8, 0, 1)
          case ( '?', D(j)) => update(j-1, j, j+1, j, 1, 1)
          case (D(c),  '?') => update(c, c+1, c, c-1, 1, 1)
          case (D(c), D(j)) => if (c != j) {
            val crest = (if (c < j) cmax else cmin)(i)
            val jrest = (if (c < j) jmin else jmax)(i)
            score = score min ((crest - jrest).abs, csofar + crest, jsofar + jrest)
            break()
          }
        }
      }
    }

    val (_, cs, js) = score
    println(s"Case #${t + 1}: %0${n}d %0${n}d".format(cs, js))
  }
