package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val b_val = b()
      b_val * b_val - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val delta = computeDelta(a, b, c)
      
      if (delta() > 0) {
        val delta_root = Math.sqrt(delta())
        Set((-b() + delta_root) / (2 * a()), (-b() - delta_root) / (2 * a()))
      } else if (delta() == 0) {
        val delta_root = Math.sqrt(delta())
        Set((-b() + delta_root) / (2 * a()))
      } else Set()
    }
  }
}
