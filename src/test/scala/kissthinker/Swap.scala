package kissthinker

object Swap extends App {
  def swap(s: String) = s.toList.grouped(2).flatMap(_.reverse).mkString

  for {
    _ <- 0 until scala.io.StdIn.readInt()
    line = scala.io.StdIn.readLine()
  } println(swap(line))
}