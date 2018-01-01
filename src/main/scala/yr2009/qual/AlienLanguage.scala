package yr2009.qual

import scala.annotation.tailrec
import scala.io.Source

import java.io.FileOutputStream

object AlienLanguage extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().takeWhile(_ != null)

  case class Dict(nodes: Map[Char, Dict]) {

    def lookup(prefix: Char): Option[Dict] = nodes.get(prefix)
    def dispatch(prefix: List[Char]): List[Dict] = prefix.flatMap(lookup)

    def add(word: String): Dict = add(word.toList)
    def add(word: List[Char]): Dict = word match {
      case Nil => this
      case let :: wrd =>
        val dict = nodes.getOrElse(let, Dict(Map.empty))
        Dict(nodes + (let -> dict.add(wrd)))
    }

    def interpretations(pattern: String): Int = {
      @tailrec def loop(
        brace: Char, pattern: List[Char], group: List[Char], dicts: List[Dict]
      ): Int = pattern match {
        case Nil => dicts.size
        case '(' :: pat => loop('(', pat, Nil, dicts)
        case ')' :: pat => loop(')', pat, Nil, dicts.flatMap(_ dispatch group))
        case let :: pat => brace match {
          case '(' => loop('(', pat, let :: group, dicts)
          case b => loop(b, pat, group, dicts.flatMap(_ lookup let))
        }
      }

      loop(' ', pattern.toList, Nil, this :: Nil)
    }
  }

  Console.withOut(new FileOutputStream(out)) {
    val d = input.next.split(' ')(1).toInt
    val dict = input.take(d).foldLeft(Dict(Map.empty))(_ add _)
    for ((line, test) <- input.zipWithIndex)
      println(s"Case #${test + 1}: ${dict interpretations line}")
  }
}
