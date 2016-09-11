package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(Math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val res1 = (-b() + Math.sqrt(delta())) / (2 * a())
    val res2 = (-b() - Math.sqrt(delta())) / (2 * a())
    Var(Set(res1, res2))
  }
}
