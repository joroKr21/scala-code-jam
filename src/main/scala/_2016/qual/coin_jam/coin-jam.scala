import scala.io.StdIn

// Don't get stuck on 1 candidate
val hardLimit = BigInt(1e12.toLong)
lazy val primes: Stream[Long] =
  2 #:: range(3, 2).filter { n =>
    primes.takeWhile(p => p * p <= n)
      .forall(n % _ != 0)
  }

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .map(_.split(' ').map(_.toInt)).zipWithIndex
  .foreach { case (Array(length, coins), t) =>
    println(s"Case #${t + 1}:")
    val jam = for {
      c <- candidates(length)
      divs = 2.to(10).toStream
        .map(BigInt(c, _))
        .map(divisor)
      if divs.forall(_.isDefined)
    } yield s"$c ${divs.flatten.mkString(" ")}"
    jam.take(coins).foreach(println)
  }

def range(from: Long, by: Long = 1): Stream[Long] =
  from #:: range(from + by, by)

def candidates(length: Int) =
  ((1l << (length - 1)) + 1)
    .until(1l << length).by(2)
    .view.map(_.toBinaryString)

def divisor(n: BigInt) = {
  val limit = n min hardLimit
  primes.takeWhile(p => p * p <= limit)
    .find(n % _ == 0)
}
