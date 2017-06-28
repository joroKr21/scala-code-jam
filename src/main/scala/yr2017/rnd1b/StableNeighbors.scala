package yr2017.rnd1b

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.io.Source

import java.io.FileOutputStream
import java.util

object StableNeighbors extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').map(_.toInt)).zipWithIndex

  val hairs = Map(
    'R' -> "R", 'O' -> "RY",
    'Y' -> "Y", 'G' -> "YB",
    'B' -> "B", 'V' -> "RB"
  ).mapValues(_.toSet)

  /*Console.withOut(new FileOutputStream(out))*/ {
    for ((Array(n, r, o, y, g, b, v), t) <- input) {
      val (rov, yog, bgv) = (r + o + v, y + o + g, b + g + v)
      print(s"Case #${t + 1}: ")
      val answer = if (Seq(rov, yog, bgv).forall(_ <= n / 2)) {
        val colors = mutable.Map('R' -> r, 'O' -> o, 'Y' -> y, 'G' -> g, 'B' -> b, 'V' -> v)
        val (max, _) = Map("ROV" -> rov, "YOG" -> yog, "BGV" -> bgv).maxBy(_._2)
        val stable = new util.LinkedList(max.toSeq.flatMap(u => Seq.fill(colors(u))(u)).asJava)
        val index = Array(1 % stable.size, 0)
        for (u <- max) colors(u) = 0
        for (u <- colors.keys) while (colors(u) > 0) {
          val neighbors = index.exists(i => (hairs(u) & hairs(stable.get(i))).nonEmpty)
          if (!neighbors) { stable.add(index.head, u); colors(u) -= 1 }
          index.transform(i => (i + (if (neighbors) 1 else 2)) % stable.size)
        }

        stable.asScala.mkString
      } else "IMPOSSIBLE"
      println(answer)
    }
  }
}
