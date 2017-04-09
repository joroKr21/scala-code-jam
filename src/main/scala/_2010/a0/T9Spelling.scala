package _2010.a0

import scala.io.Source

import java.io.FileOutputStream

object T9Spelling extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).zipWithIndex

  val layout   = Iterator(" ", "", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz")
  val alphabet = (for {
    (letters, digit) <- layout.zipWithIndex
    (letter, n) <- letters.iterator.zipWithIndex
  } yield letter -> digit.toString * (n + 1)).toMap

  Console.withOut(new FileOutputStream(out))(for {
    (message, t) <- input
    encoded = interleave(message.toStream.map(alphabet)) {
      (a, b) => if (a.last == b.head) " " else ""
    }.mkString
  } println(s"Case #${t + 1}: $encoded"))

  def interleave[A](xs: Stream[A])(f: (A, A) => A): Stream[A] = xs match {
    case fst #:: (tail @ snd #:: _) => fst #:: f(fst, snd) #:: interleave(tail)(f)
    case _ => xs
  }
}
