package _2015.qual

import scala.io.Source

import java.io.FileOutputStream

object Dijkstra extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).grouped(2).zipWithIndex

  val Q = Map('1' -> 1, 'i' -> 2, 'j' -> 3, 'k' -> 4).withDefaultValue(0)
  val M = Vector(
    Vector(1,  2,  3,  4), Vector(2, -1,  4, -3),
    Vector(3, -4, -1,  2), Vector(4,  3, -2, -1))

  Console.withOut(new FileOutputStream(out))(for {
    (Seq(ln, qs), t) <- input
    n = ln.split(' ').last.toLong
    quaternions = qs.map(Q)
    prod = pow(quaternions.foldLeft(1)(mul), n)
  } {
    def prefix = Vector.fill((n min 8).toInt)(quaternions).flatten
    def i = prefix.scanLeft(1)(mul).indexOf(2)
    def j = prefix.drop(i).scanLeft(1)(mul).indexOf(3)
    val ijk = if (prod == -1 && i >= 0 && j >= 0) "YES" else "NO"
    println(s"Case #${t + 1}: $ijk")
  })

  def mul(p: Int, q: Int) =
    ((p * q) compare 0) * M(p.abs - 1)(q.abs - 1)

  def pow(q: Int, n: Long) =
    Iterator.fill((n % 4).toInt)(q).foldLeft(1)(mul)
}
