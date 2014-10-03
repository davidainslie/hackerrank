package kissthinker

import java.io.ByteArrayOutputStream
import org.specs2.mutable.Specification

class IntroductionSpec extends Specification {
  "My solution" should {
    """update the values of a list with their absolute values""" in {
      def f(arr: List[Int]): List[Int] = arr.map { i =>
        if (i < 0) -i else i
      }

      f((-5 to 5).toList) must contain(exactly(5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5))
    }

    """count the number of elements in an array without using count, size or length operators (or their equivalents)""" in {
      def f(arr: List[Int]): Int = arr.foldLeft(0) { (acc, _) => acc + 1 }

      f(Nil) mustEqual 0
      f(List(1, 2, 3, 10)) mustEqual 4
    }

    """create an array of N integers, where N is specified as input. This can be any array with N integers.
       For example, for N = 4 you could return [1, 1, 1, 1] or [1, 2, 3, 4]""" in {
      def f(num: Int): List[Int] = List.fill(num)(1)

      f(5) mustEqual List(1, 1, 1, 1, 1)
      f(0) mustEqual Nil
      f(-1) mustEqual Nil

      def ff(num: Int): List[Int] = List.tabulate(num)(i => i)

      ff(5) mustEqual List(0, 1, 2, 3, 4)
    }

    """A "Hello World" program has been widely adopted as the introductory program for learning programming.
       Sample Input: (Nil)
       Sample Output: Hello World""" in {
      def f() = println("Hello World")

      val stream = new ByteArrayOutputStream()

      Console.withOut(stream) {
        f()
      }

      stream.toString mustEqual "Hello World\n"
    }

    """Filter a given array of integers and leave only that values which are less than a specified value X, without using inbuilt 'filter'""" in {
      def f(delim: Int, arr: List[Int]): List[Int] = arr.foldRight(List.empty[Int]) { (x, acc) => if (x < delim) x +: acc else acc }

      f(3, List(3, 10, 9, 8, 2, 7, 5, 1, 3, 0)) must contain(exactly(2, 1, 0))
    }

    """Given a list repeat each element of the list n times""" in {
      def f(num: Int, arr: List[Int]): List[Int] = arr flatMap { i => List.fill(num)(i) }

      f(3, List(1, 2, 3, 4)) must contain(exactly(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4))
    }

    """Reverse a list without using reverse function""" in {
      def f(arr: List[Int]): List[Int] = arr.foldLeft(List.empty[Int]) { (acc, i) => i +: acc }

      f(List(19, 22, 3, 28, 26, 17, 18, 4, 28, 0)) must contain(exactly(0, 28, 4, 18, 17, 26, 28, 3, 22, 19))
    }

    """Return sum of odd elements from an list""" in {
      def f(arr: List[Int]): Int = arr.filterNot(_ % 2 == 0).sum

      f(List(2, 3, 4, 6, 5, 7, 8, 0, 1)) mustEqual 16
      f(Nil) mustEqual 0
    }

    """For a given list with N integers, return a new list containing the elements from odd indices""" in {
      def f(arr: List[Int]): List[Int] = arr.zipWithIndex filterNot { case (x, i) => i % 2 == 0 } map { case (x, i) => x }

      f(List(2, 5, 3, 4, 6, 7, 9, 8)) must contain(exactly(5, 4, 7, 8))
      f(Nil) must beEmpty
    }

    """Print "Hello World" N times""" in {
      def f(n: Int) = (0 until n) foreach { _ => println("Hello World") }

      val stream = new ByteArrayOutputStream()

      Console.withOut(stream) {
        f(4)
      }

      stream.toString mustEqual "Hello World\nHello World\nHello World\nHello World\n"
    }

    """The series expansion of e^x is given by:
       1 + x + x^2/2! + x^3/3! + x^4/4! + .......
       which is:
       1 + x^1/1! + x^2/2! + x^3/3! + x^4/4! + .......
       Evaluate e^x for given values of x, by using the above expansion for the first 10 terms""" in {
      def f(x: Float): Float = {
        def next(n: BigDecimal): Stream[BigDecimal] = {
          def factorial(n: BigDecimal): BigDecimal =
            if (n <= 1) n
            else factorial(n - 1) * n

          (math.pow(x, n.toDouble) / factorial(n)) #:: next(n + 1)
        }

        (1 #:: next(1)).take(10).foldLeft(BigDecimal(0.0)) { (acc, a) => acc + a }.toFloat
      }

      f(20.0000F) must beBetween((2423600.1887 - 0.1).toFloat, (2423600.1887 + 0.1).toFloat)
    }

    // TODO
    """Consider the algebraic expression given by:
       (a1)x^b1 + (a2)x^b2 + (a3)x^b3 ......(an)x^bn
       1. Evaluate the area bounded by a given polynomial function of the kind described above, between given limits a and b.
       2. Evaluate the volume of the solid obtained by revolving this polynomial curve around the X-Axis.""" in {

      /**
       * This function will be used while invoking "Summation" to compute
       * The area under the curve.
       */
      def f(coefficients: List[Int], powers: List[Int], x: Double): Double =
        (coefficients zip powers map { case (c, p) => c * math.pow(x, p) }).sum

      /**
       * This function will be used while invoking "Summation" to compute
       * The Volume of revolution of the curve around the X-Axis
       * The 'Area' referred to here is the area of the circle obtained
       * By rotating the point on the curve (x,f(x)) around the X-Axis
       */
      def area(coefficients: List[Int], powers: List[Int], x: Double): Double =
        math.Pi * math.pow(f(coefficients, powers, x), 2)

      /**
       * This is the part where the series is summed up
       * This function is invoked once with func = f to compute the area under the curve
       * Then it is invoked again with func = area to compute the volume of revolution of the curve
       */
      def summation(func: (List[Int], List[Int], Double) => Double,
                    upperLimit: Int, lowerLimit: Int,
                    coefficients: List[Int], powers: List[Int]): Double =
        ((lowerLimit * 1000) to (upperLimit * 1000) map { e => func(coefficients, powers, e * 0.001) * 0.001 }).sum

      summation(f, -1000, 1000, List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10)) mustEqual 2435300.3 // TODO WRONG
    }
  }
}