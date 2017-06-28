package yr2016.rnd1c

import scala.io.Source

import java.io.FileOutputStream

object FashionPolice extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').view.map(_.toInt)).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (Seq(jeans, pants, shirts, k), t) <- input
    cycle = Seq.fill(3)(1 to shirts).flatten
    wear  = for {
      jean <- 1 to jeans
      pant <- 1 to pants
      from  = jean + pant
      until = from + (shirts min k)
      shirt <- cycle.slice(from, until)
    } yield s"$jean $pant $shirt"
    _ = println(s"Case #${t + 1}: ${wear.size}")
    clothing <- wear
  } println(clothing))
}
