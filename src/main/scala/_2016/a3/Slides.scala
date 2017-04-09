package _2016.a3

import scala.io.Source

import java.io.FileOutputStream

object Slides extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input =  Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).map(_ split ' ').zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (Array(b, w), t) <- input
    buildings = b.toInt
    ways      = w.toLong
    maxWays   = 1l << buildings - 2
    _ = print(s"Case #${t + 1}: ")
  } if (ways > maxWays) println("IMPOSSIBLE") else {
    println("POSSIBLE")
    val first = padLeftTo(buildings, '0') {
      if (ways == maxWays) fill(buildings - 1)('1')
      else (ways << 1).toBinaryString
    }
    val between = for (b <- 2 until buildings)
      yield fill(b)('0').padTo(buildings, '1')
    val last = fill(buildings)('0')
    (first +: between :+ last) foreach println
  })

  def fill(n: Int)(c: Char) =
    new String(Array.fill(n)(c))

  def padLeftTo(n: Int, c: Char)(str: String) =
    fill(n - str.length)(c) + str
}
