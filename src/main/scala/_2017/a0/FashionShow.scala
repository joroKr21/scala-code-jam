package _2017.a0

import scala.io.Source

import java.io.FileOutputStream

// FIXME: Incorrect
object FashionShow extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).map(_ split ' ')

  val score = Map('.' -> 0, '+' -> 1, 'x' -> 1, 'o' -> 2)

  Console.withOut(new FileOutputStream(out)) {
    var t = 0
    while (input.hasNext) {
      t += 1
      val Array(n, m) = input.next.map(_.toInt)
      val grid   = Array.fill(n, n)('.')
      val models = input.take(m).map {
        case Array(k, i, j) =>
          val i0  = i.toInt - 1
          val j0  = j.toInt - 1
          val mod = k.head
          grid(i0)(j0) = mod
          (i0, j0) -> mod
      }.toMap.withDefaultValue('.')

      val index = for {
        i <- grid.indices
        j <- grid.indices
      } yield (i, j)

      val idxSet = grid.indices.toSet
      var rooks  = grid.indices.map(_ -> idxSet).toMap

      for {
        ((i, j), mod) <- models.iterator
        if mod == 'x' || mod == 'o'
      } rooks = (rooks - i).mapValues(_ - j)

      while (rooks.nonEmpty) {
        val (i, js) = rooks.head
        val j = js.head
        grid(i)(j) = if (grid(i)(j) == '+') 'o' else 'x'
        rooks = (rooks - i).mapValues(_ - j)
      }

      def diagonal(i: Int, j: Int) = {
        val sum = i + j
        val col = true.compare(sum % 2 == 0)
        val off = (n - col - 1) / 2 - sum / 2
        (col, sum / 2, j + off)
      }

      val bishops = Array.fill(2) {
        Map.empty[Int, Map[Int, (Int, Int)]]
          .withDefaultValue(Map.empty)
      }

      for {
        (i, j) <- index
        (col, k, l) = diagonal(i, j)
        bsk = bishops(col)(k)
        lij = l -> (i, j)
      } bishops(col) += k -> (bsk + lij)

      for {
        (i, j) <- index
        if "+o" contains grid(i)(j)
        (col, k, l) = diagonal(i, j)
      } bishops(col) = (bishops(col) - k).mapValues(_ - l)

      for {
        col <- bishops.indices
        k   <- bishops(col).keys.toSeq
          .sortBy(k => bishops(col)(k).size)
      } if (bishops(col)(k).nonEmpty) {
        val (l, (i, j)) = bishops(col)(k).head
        grid(i)(j)   = if (grid(i)(j) == 'x') 'o' else '+'
        bishops(col) = (bishops(col) - k).mapValues(_ - l)
      }

      val (style, subst, arrangement) =
        index.foldLeft(0, 0, List.empty[String]) {
          case ((y, z, acc), (i, j)) =>
            val mod = grid(i)(j)
            val old = mod == models(i, j)
            (y + score(mod), z + true.compare(old),
              if (old) acc else s"$mod ${i + 1} ${j + 1}" :: acc)
        }

      println(s"Case #$t: $style $subst")
      arrangement.foreach(println)
    }
  }
}