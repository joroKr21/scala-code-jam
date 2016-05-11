import scala.io.StdIn

val parties = 'A' to 'Z'

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null).grouped(2)
  .map(_.last.split(' ').map(_.toInt))
  .zipWithIndex.foreach { case (senators, t) =>
    val n = senators.length
    var total = senators.sum
    var evacuated = 0
    print(s"Case #${t + 1}:")
    while (total > n) {
      if (evacuated % 2 == 0) print(' ')
      val evac = senators.indices maxBy senators
      senators(evac) -= 1
      total -= 1
      evacuated += 1
      print(parties(evac))
    }

    for (evac <- 0 until n - 2) {
      if (evacuated % 2 == 0) print(' ')
      evacuated += 1
      print(parties(evac))
    }

    println(parties.slice(n - 2, n).mkString(" ", "", ""))
  }

