package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  val currentRefs = scala.collection.mutable.Set[String]()
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    def resolveNamedExpr(kv: (String, Signal[Expr])): Signal[Double] = {
      Signal {
        val expr = kv._2
        eval(expr(), namedExpressions)
      }
    }
    println(s"computing: $namedExpressions")

    namedExpressions.foldLeft(Map[String, Signal[Double]]())((m, kv) => {
      m + (kv._1 -> resolveNamedExpr(kv))
    })

  }



  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case (e:Literal) => {
        e.v
      } case (e:Ref) => {
        val found = getReferenceExpr(e.name, references)
        if (currentRefs.contains(e.name)) {
          Double.NaN
        } else {
          currentRefs += e.name
          val res: Double = eval(found, references)
          currentRefs -= e.name
          res
        }
      } case (e:Plus) => {
        eval(e.a, references) + eval(e.b, references)
      } case (e:Minus) => {
        eval(e.a, references) - eval(e.b, references)
      } case (e:Times) => {
        eval(e.a, references) * eval(e.b, references)
      } case (e:Divide) => {
        eval(e.a, references) / eval(e.b, references)
      }
    }
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
}
