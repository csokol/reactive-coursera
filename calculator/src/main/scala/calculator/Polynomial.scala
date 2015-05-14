package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val b1 = b()
      b1*b1 - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val b1 = b()
      val delta1 = delta()
      val a1 = a()
      val c1 = c()
      println(s"a:$a1, b:$b1, c:$c1")
      val s1 = (-b1 + Math.sqrt(delta1)) / (2*a1)
      val s2 = (-b1 - Math.sqrt(delta1)) / (2*a1)
      Set(s1, s2)
    }
  }
}
