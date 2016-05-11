import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .map(_.split(' ').map(_.toInt)).zipWithIndex
  .foreach { case (Array(jeans, pants, shirts, k), t) =>
    val cycle = Seq.fill(3)(1 to shirts).flatten
    val wear = for {
      jean <- 1 to jeans
      pant <- 1 to pants
      from = jean + pant
      until = from + (shirts min k)
      shirt <- cycle.slice(from, until)
    } yield s"$jean $pant $shirt"
    println(s"Case #${t + 1}: ${wear.size}")
    println(wear mkString "\n")
  }
