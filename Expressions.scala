package csp.ch01
//Oleg Puchkov 


object Expressions {
  
  sealed trait Expr
  case class CstI (n:Int)                       extends Expr
  case class Var (s:String)                       extends Expr
  case class Prim (op:String, e1:Expr, e2:Expr) extends Expr
  case class If   (e1:Expr, e2:Expr, e3:Expr)   extends Expr

  def eval (e:Expr) : Int = {
    e match {
      case CstI (n)           => n
      case Prim ("+", e1, e2) => eval (e1) + eval (e2)
      case Prim ("*", e1, e2) => eval (e1) * eval (e2)
      case Prim ("-", e1, e2) => eval (e1) - eval (e2)
      case Prim ("max", e1, e2) => {val x = eval(e1); val y = eval(e2); if (x > y) x else y}
      case Prim ("min", e1, e2) => {val x = eval(e1); val y = eval(e2); if (x > y) y else x}
      case Prim ("==", e1, e2) => if (eval(e1) == eval(e2)) 1 else 0
      case Prim ( _, _, _) => throw new RuntimeException ("unknown primitive")
      case If   (e1, e2, e3) => if (eval(e1) != 0) eval(e2) else eval(e3)
    }
  }
 //(III)
  def eval1 (e:Expr) : Int = {
    e match {
      case CstI (n)          => n
      case Prim (op, e1, e2) =>
        val v1 = eval(e1)
        val v2 = eval(e2)
        op match {
          case "+"   => v1 + v2
          case "*"   => v1 * v2
          case "-"   => v1 - v2
          case "max" => if (v1 > v2) v1 else v2
          case "min" => if (v1 < v2) v1 else v2
          case "=="  => if (v1 == v2) 1 else 0
        }
      case Prim ( _, _, _)   => throw new RuntimeException ("unknown primitive")
      case If   (e1, e2, e3) => if (eval(e1) != 0) eval(e2) else eval(e3)
    }
  }
  
  def evalm (e:Expr) : Int = {
    e match {
      case CstI (n)           => n
      case Prim ("+", e1, e2) => evalm (e1) + evalm (e2)
      case Prim ("*", e1, e2) => evalm (e1) * evalm (e2)
      case Prim ("-", e1, e2) => {
        val res = evalm (e1) - evalm (e2)
        if (res < 0) 0 else res
      }
      case Prim ( _, _, _) => throw new RuntimeException ("unknown primitive")
    }
  }

}
