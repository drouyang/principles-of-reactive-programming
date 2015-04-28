package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map {
      case (k, v) => k -> Signal(eval(v(), namedExpressions))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match{
    case Literal(v) => v
    case Ref(name) => _eval(getReferenceExpr(name, references), List(name), references)
    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
  }

  /* JO: evaluation with cyclic dependency check */
  private def _eval(expr: Expr, 
      dep: List[String], 
      references: Map[String, Signal[Expr]]): Double = expr match {
    case Ref(name) => 
      if (dep.contains(name)) { println(name, dep); Double.NaN }
      else { _eval(getReferenceExpr(name, references), dep :+ name, references) }
    case Plus(a, b) => _eval(a, dep, references) + _eval(b, dep, references)
    case Minus(a, b) => _eval(a, dep, references) - _eval(b, dep, references)
    case Times(a, b) => _eval(a, dep, references) * _eval(b, dep, references)
    case Divide(a, b) => _eval(a, dep, references) / _eval(b, dep, references)
    case Literal(v) => v
  }
  
  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
  
  /* for test */
  def main(args: Array[String]) {
      computeValues(Map("a" -> Signal(Ref("b")), "b" -> Signal(Ref("a")), "c" -> Signal(Ref("c"))))
  }
}
