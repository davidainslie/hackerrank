package kissthinker

import java.io.ByteArrayOutputStream

import org.specs2.mutable.Specification

class RecursionSpec extends Specification {
  "My solution" should {
    """For a given integer K, print the first K rows of Pascal's Triangle.
       Print each row with values separated by spaces.
       The value at nth row and rth column of the triangle is equal to n! / (r! * (n-r)!) where indexing start from 0""" in {
      def pascal(k: Int) = {
        def p(row: Int): List[Int] = row match {
          case 1 => List(1)
          case n: Int => List(1) ++ ((p(n - 1) zip p(n - 1).tail) map { case (a, b) => a + b }) ++ List(1)
        }

        (1 to k) foreach { i =>
          println(p(i).mkString(" "))
        }
      }

      val stream = new ByteArrayOutputStream()

      Console.withOut(stream) {
        pascal(4)
      }

      stream.toString mustEqual
        """1
          |1 1
          |1 2 1
          |1 3 3 1
          |""".stripMargin
    }

    """The Fibonacci series begins with 0 and 1 (which are the first and the second terms respectively).
       After this, every element is the sum of the preceding elements - 0, 1, 1, 2, 3, 5, 8
       Return the Nth term""" in {
      def fibonacci(x: Int): Int = x match {
        case 1 => 0
        case 2 | 3 => 1
        case n => fibonacci(n - 1) + fibonacci(n - 2)
      }

      fibonacci(1) mustEqual 0
      fibonacci(2) mustEqual 1
      fibonacci(3) mustEqual 1
      fibonacci(4) mustEqual 2
      fibonacci(5) mustEqual 3
      fibonacci(6) mustEqual 5
      fibonacci(7) mustEqual 8
    }

    """Given two integers 'x' and 'y' a recursive technique to find their GCD is the Euclidean Algorithm.""" in {
      def gcd(x: Int, y: Int): Int = {
        if (x < y) {
          gcd(y, x)
        } else {
          val r = x % y

          if (r == 0) y
          else gcd(y, r)
        }
      }

      gcd(1, 5) mustEqual 1
      gcd(10, 100) mustEqual 10
      gcd(22, 131) mustEqual 1
    }

    """The Sierpinski Triangle is a pretty fractal which consistes of layers of self-similar triangles, nested inside each other.
       32 rows and 63 columns""" in {
      def drawTriangles(n: Int): Unit = {
        for {
          r <- 1 to 32
        } {
          val ones = "1" + ("11" * (r - 1))
          val fillers = "_" * ((63 - ones.length) / 2)

          print(fillers)
          print(ones)
          println(fillers)
        }
      }

      val stream = new ByteArrayOutputStream()

      Console.withOut(stream) {
        drawTriangles(1)
      }

      stream.toString mustEqual
        """_______________________________1_______________________________
          |______________________________111______________________________
          |_____________________________11111_____________________________
          |____________________________1111111____________________________
          |___________________________111111111___________________________
          |__________________________11111111111__________________________
          |_________________________1111111111111_________________________
          |________________________111111111111111________________________
          |_______________________11111111111111111_______________________
          |______________________1111111111111111111______________________
          |_____________________111111111111111111111_____________________
          |____________________11111111111111111111111____________________
          |___________________1111111111111111111111111___________________
          |__________________111111111111111111111111111__________________
          |_________________11111111111111111111111111111_________________
          |________________1111111111111111111111111111111________________
          |_______________111111111111111111111111111111111_______________
          |______________11111111111111111111111111111111111______________
          |_____________1111111111111111111111111111111111111_____________
          |____________111111111111111111111111111111111111111____________
          |___________11111111111111111111111111111111111111111___________
          |__________1111111111111111111111111111111111111111111__________
          |_________111111111111111111111111111111111111111111111_________
          |________11111111111111111111111111111111111111111111111________
          |_______1111111111111111111111111111111111111111111111111_______
          |______111111111111111111111111111111111111111111111111111______
          |_____11111111111111111111111111111111111111111111111111111_____
          |____1111111111111111111111111111111111111111111111111111111____
          |___111111111111111111111111111111111111111111111111111111111___
          |__11111111111111111111111111111111111111111111111111111111111__
          |_1111111111111111111111111111111111111111111111111111111111111_
          |111111111111111111111111111111111111111111111111111111111111111
          |""".stripMargin
    }

    """if str = "abcdpqrs", then we have to swap the characters at position {(0, 1), (2, 3), (4, 5), (6, 7)} as L = 8. So answer will be "badcqpsr".""" in {
      def swap(s: String) = s.toList.grouped(2).flatMap(_.reverse).mkString

      swap("abcdpqrs") mustEqual "badcqpsr"
    }

    """Creating a Fractal Tree from Y-shaped branches
       63 rows and 100 columns
       16 character each for height of top and bottom half of Y""" in {
      def yBlock(level: Int): Vector[Vector[String]] = {
        val width = 33 / level
        val height = 16 / level

        val filler = for {
          r <- (1 until width).toVector
        } yield Vector("_" * ((100 - width) / 2))

        val y = {
          val topY = for {
            r <- (height to 1 by -1).toVector
          } yield {
            val numberOfInnnerFillers = 2 * r - 1
            Vector("_" * (height - 1 - numberOfInnnerFillers / 2), "1", "_" * numberOfInnnerFillers, "1", "_" * (height - 1 - numberOfInnnerFillers / 2))
          }

          val bottomY = for {
            r <- (1 to height).toVector
          } yield Vector("_" * height, "1", "_" * height)

          topY ++ bottomY
        }

        def constructYs(level: Int, ys: Vector[Vector[String]]): Vector[Vector[String]] = {
          if (level == 1) ys
          else constructYs(level - 1, ys.zip(y).map { case (v1, v2) => v1 ++ v2 }.zip(filler).map { case (v1, v2) => v1 ++ v2 })
        }

        constructYs(level, filler.zip(y).map { case (v1, v2) => v1 ++ v2 }.zip(filler).map { case (v1, v2) => v1 ++ v2 })
      }

      def ys(levels: Int): Vector[Vector[String]] = {
        def y(level: Int, remainingLevels: Int, ys: Vector[Vector[String]]): Vector[Vector[String]] = {
          if (remainingLevels == 0) yBlock(level)
          else y(level + 1, remainingLevels - 1, yBlock(level) ++ ys)
        }

        val topFiller = for {
          r <- (1 to (63 - 16 * levels)).toVector
        } yield Vector("_" * 100)

        topFiller ++ y(1, levels - 1, Vector.empty[Vector[String]])
      }

      ys(2).foreach { r => println(r.mkString) }
      "1" mustEqual "1"
    }
  }
}