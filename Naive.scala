package csp.ch07

// From SBT: ~run-main csp.ch07.Naive

// Based on the parser/interpreter in directory Imp from the Sestoft source code.

object Naive {

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Syntax
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Expr
  case class CstI (n : Int)                                           extends Expr
  case class Var (nm : String)                                        extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr)                 extends Expr

  sealed trait Stmt
  case class Asgn (nm : String, e : Expr)                         extends Stmt
  case class If (e : Expr, s1 : Stmt, s2 : Stmt)                  extends Stmt
  case class Block (ss : List[Stmt])                              extends Stmt
  case class For (nm : String, low : Expr, high : Expr, s : Stmt) extends Stmt
  case class While (e : Expr, s : Stmt)                           extends Stmt
  case class Print (e : Expr)                                     extends Stmt

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Evaluation
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // (* A naive store is a map from names (strings) to values (ints) *)

  type NaiveStore = Map[String,Int]

  val emptyStore : NaiveStore = Map.empty

  def getSto (store : NaiveStore, x : String) : Int = {
    store.get (x).get
  }

  def setSto (store : NaiveStore, k : String, v : Int) : NaiveStore = {
    store + ( (k, v) )
  }

  def b2i (b : Boolean) : Int = if (b) 1 else 0

  def eval (e : Expr, store : NaiveStore) : Int = {
    e match {
      case CstI (i)           => i
      case Var (x)            => getSto (store, x)
      case Prim (op, e1, e2) => {
        val i1 = eval (e1, store) 
        val i2 = eval (e2, store)
        op match {
          case  "+" => i1 + i2
          case  "-" => i1 - i2
          case  "*" => i1 * i2
          case "==" => b2i (i1 == i2) 
          case "<>" => b2i (i1 != i2) 
          case  "<" => b2i (i1 < i2) 
          case  ">" => b2i (i1 > i2) 
          case "<=" => b2i (i1 <= i2) 
          case ">=" => b2i (i1 >= i2) 
          case   _ => throw new RuntimeException ("unknown primitive " + op)
        }
      }
    }
  }

  def exec (s : Stmt, store : NaiveStore) : NaiveStore = {
    s match {
      case Asgn (nm, e)            => {
        val v : Int = eval (e, store)
        // println ("store is %s".format (store))
        // println ("assigning %d to %s".format (v, nm))
        setSto (store, nm, v)
      }
      case If (e, s1, s2)          => exec (if (eval (e, store) != 0) s1 else s2, store)
      case Block (ss)              => {
        def loop (ss2 : List[Stmt], store2 : NaiveStore) : NaiveStore = {
          ss2 match {
            case Nil       => store2
            case s2 :: ss3 => loop (ss3, exec (s2, store2))
          }
        }
        loop (ss, store)
      }
      case For (nm, low, high, s)  => {
        val start : Int = eval (low, store) 
        val stop : Int = eval (high, store)
        def loop (i : Int, sto : NaiveStore) : NaiveStore = {
          if (i > stop) {
            sto 
          } else {
            loop (i + 1, exec (s, setSto (sto, nm, i)))
          }
        }
        loop (start, store)
      }
      case While (e, s)            => {
        def loop (sto : NaiveStore) : NaiveStore = {
          if (eval (e, sto) != 0) {
            loop (exec (s, sto))
          } else {
            sto
          }
        }
        loop (store)
      }
      case Print (e)               => {
        println (eval (e, store))
        store
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Testing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def run (s : Stmt) : Unit = {
    exec (s, emptyStore)
  }

  // (* Example programs *)

  val ex1 : Stmt = {
    Block (
      List (
        Asgn (
          "sum", 
          CstI (0)),
        For (
          "i", 
          CstI (0), 
          CstI (100), 
          Asgn (
            "sum", 
            Prim ("+", Var ("sum"), Var ("i"))
          )),
        Print (Var ("sum"))
      )
    )
  }

  val ex2 : Stmt = {
    Block (
      List (
        Asgn (
          "i", 
          CstI (1)
        ),
        Asgn (
          "sum", 
          CstI (0)
        ),
        While (
          Prim ("<", Var ("sum"), CstI (10000)),
          Block (
            List (
              Print (Var ("sum")),
              Asgn (
                "sum", 
                Prim ("+", Var ("sum"), Var ("i"))
              ),
              Asgn (
                "i", 
                Prim ("+", CstI (1), Var ("i"))
              )
            )
          )
        ),
        Print (Var ("i")),
        Print (Var ("sum"))
      )
    )
  }

  def main (args : Array[String]) {
    println ("=" * 80)
    
    run (ex1)
    println ("=" * 80)

    run (ex2)
    println ("=" * 80)
  }
}