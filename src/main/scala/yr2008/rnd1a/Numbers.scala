package yr2008.rnd1a

import scala.annotation.tailrec
import scala.io.Source

import java.io.FileOutputStream

object Numbers extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).zipWithIndex
  val recur = Array(Array(3, 5), Array(1, 3))
  val init  = Array(Array(1), Array(0))

  type Matrix = Array[Array[Int]]
  implicit class MatrixOps(val A: Matrix) extends AnyVal {
    def rows: Int = A.length
    def cols: Int = A.head.length
    def rowIdx: Range = 0 until rows
    def colIdx: Range = 0 until cols

    def *(B: Matrix): Matrix = {
      require(A.cols == B.rows)
      val C = Array.fill(A.rows, B.cols)(0)
      for (i <- A.rowIdx; j <- B.colIdx; k <- A.colIdx)
        C(i)(j) += A(i)(k) * B(k)(j)

      C
    }

    def :%(mod: Int): Matrix =
      for (i <- rowIdx.toArray) yield
        for (j <- colIdx.toArray) yield A(i)(j) % mod

    def modPow(exp: Int, mod: Int): Matrix = {
      require(rows == cols)
      val ones = Array.tabulate(rows, cols) {
        (i, j) => if (i == j) 1 else 0
      }

      @tailrec def modPow(A: Matrix, exp: Int, aux: Matrix): Matrix =
        if (exp == 0) ones
        else if (exp == 1) aux * A :% mod
        else if (exp % 2 == 0) modPow(A * A :% mod, exp / 2, aux)
        else modPow(A, exp - 1, aux * A :% mod)

      modPow(A, exp, ones)
    }
  }

  Console.withOut(new FileOutputStream(out)) {
    for ((n, test) <- input) {
      val result = recur.modPow(n.toInt, 1000) * init
      val digits = (2 * result.head.head + 999) % 1000
      println(f"Case #${test + 1}%d: $digits%03d")
    }
  }
}
