import scala.io.StdIn

val M = Vector(
  Vector(1,  2,  3,  4),
  Vector(2, -1,  4, -3),
  Vector(3, -4, -1,  2),
  Vector(4,  3, -2, -1))

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .grouped(2).zipWithIndex
  .foreach { case (Seq(ln, qs), t) =>
    val n = ln.split(' ').last.toLong
    val quaternions = qs.map(quaternion)
    val prod = pow(quaternions.foldLeft(1)(mul), n)
    lazy val prefix = Vector.fill((n min 8).toInt)(quaternions).flatten
    lazy val i = prefix.scanLeft(1)(mul).indexOf(2)
    lazy val j = prefix.drop(i).scanLeft(1)(mul).indexOf(3)
    val ijk = if (prod == -1 && i >= 0 && j >= 0) "YES" else "NO"
    println(s"Case #${t + 1}: $ijk")
  }

def quaternion(q: Char) = q match {
  case '1' => 1
  case 'i' => 2
  case 'j' => 3
  case 'k' => 4
  case _   => 0
}

def mul(p: Int, q: Int) =
  ((p * q) compare 0) * M(p.abs - 1)(q.abs - 1)

def pow(q: Int, n: Long) = Iterator
  .fill((n % 4).toInt)(q)
  .foldLeft(1)(mul)
