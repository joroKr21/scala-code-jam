package _2017.b

import scala.io.Source

import java.io.FileOutputStream

object FreshChocolate extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').map(_.toInt)).grouped(2).zipWithIndex

  def ceilDiv(x: Int, y: Int) = x / y + (x % y).signum

  Console.withOut(new FileOutputStream(out)) {
    for ((Seq(Array(_, p), groups), t) <- input) {
      val freq = groups.foldLeft(Vector.fill(p)(0)) { (fq, g) =>
        val i = g % p
        fq.updated(i, fq(i) + 1)
      }

      val fresh = freq match {
        case Vector(zero, one) =>
          zero + ceilDiv(one, 2)
        case Vector(zero, one, two) =>
          zero + (one min two) + ceilDiv((one - two).abs, 3)
        case Vector(zero, one, two, three) =>
          zero + two / 2 + (one min three) +
            ceilDiv(2 * (two % 2) + (one - three).abs, 4)
        case _ => throw new IndexOutOfBoundsException(s"p = $p > 4")
      }

      println(s"Case #${t + 1}: $fresh")
    }
  }
}
