import scala.io.StdIn

val layout = Iterator(" ", "", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz")
val alphabet = (for {
  (letters, digit) <- layout.zipWithIndex
  (letter, n) <- letters.view.zipWithIndex
} yield letter -> digit.toString * (n + 1)).toMap

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null).zipWithIndex
  .foreach { case (message, t) =>
    val encoded = interleave(message.toStream map alphabet) {
      (a, b) => if (a.last == b.head) " " else ""
    }.mkString
    println(s"Case #${t + 1}: $encoded")
  }

def interleave[A](str: Stream[A])(f: (A, A) => A): Stream[A] = str match {
  case fst #:: (tail @ snd #:: _) =>
    fst #:: f(fst, snd) #:: interleave(tail)(f)
  case _ => str
}
