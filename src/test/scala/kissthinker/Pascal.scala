package kissthinker

object Pascal extends App {
  def pascal(k: Int) = {
    def p(row: Int): List[Int] = row match {
      case 1 => List(1)
      case n: Int => List(1) ++ ((p(n - 1) zip p(n - 1).tail) map { case (a, b) => a + b }) ++ List(1)
    }

    (1 to k) foreach { i =>
      println(p(i).mkString(" "))
    }
  }

  for (line <- io.Source.stdin.getLines()) pascal(line.toInt)
}