package _2016.a0

import scala.io.Source

import java.io.FileOutputStream

object Fractiles extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').view.map(_.toInt)).zipWithIndex

  val one = BigInt(1)
  Console.withOut(new FileOutputStream(out))(for {
    (Seq(tiles, complexity, students), t) <- input
    t1 = t + 1
  } if (complexity * students < tiles) {
    println(s"Case #$t1: IMPOSSIBLE")
  } else {
    val necessary = tiles min complexity
    var infoGain  = 0
    var check     = List.empty[BigInt]
    var tile      = one
    while (infoGain < tiles) {
      var level = 1
      while (level < necessary) {
        level += 1
        tile = (tile - one) * tiles + tile % tiles + one
      }

      infoGain += level
      check   ::= tile
      tile      = infoGain + 1
    }

    println(s"Case #$t1: ${check.reverse.mkString(" ")}")
  })

  def fractile(art: String, complexity: Int) = {
    val gold = new String(Array.fill(art.length)('G'))
    def step(level: String) = level.map {
      case 'G' => gold
      case _   => art
    }.mkString("")
    Iterator.iterate(art)(step).drop(complexity - 1).next
  }
}
