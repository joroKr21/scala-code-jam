import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .zipWithIndex.foreach { case (word, t) =>
    val lastWord = new String(word.tail
      .foldLeft(Vector(word.head)) { (last, ch) =>
        if (ch < last.head) last :+ ch else ch +: last
      }.toArray)
    println(s"Case #${t + 1}: $lastWord")
  }
