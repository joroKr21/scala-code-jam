package _2017.a1

import scala.io.Source

import java.io.FileOutputStream

object AlphabetCake extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)

  Console.withOut(new FileOutputStream(out)) {
    var t = 0
    while (input.hasNext) {
      t += 1
      val Array(r, c) = input.next.split(' ').map(_.toInt)
      val grid = input.take(r).map(_.toCharArray).toArray

      for { // left to right
        row <- grid.view
        i <- row.indices.tail
        if row(i) == '?'
      } row(i) = row(i - 1)

      for { // right to left
        row <- grid.view
        i <- row.indices.reverse.tail
        if row(i) == '?'
      } row(i) = row(i + 1)

      for { // top to bottom
        i <- grid.indices.tail
        if grid(i).head == '?'
      } grid(i) = grid(i - 1)

      for { // bottom to top
        i <- grid.indices.reverse.tail
        if grid(i).head == '?'
      } grid(i) = grid(i + 1)

      println(s"Case #$t:")
      grid.foreach(row => println(new String(row)))
    }
  }
}