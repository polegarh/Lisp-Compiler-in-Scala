/*package csp.Project

// From SBT: ~run-main csp.Project


object Project {
  object MyParsersNoWhitespace {
    import fastparse.all._
    /
    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val keywords : List[String] = List ("let", "in", "end", "quote")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
    val variable : Parser[Expr] = ident.map (s => Var (s))
    
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace (" ".rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val putYourStuffHere : Parser[Unit] = P ("do some stuff here")

    val wspcP = P(CharPred(_.isWhitespace).rep)
    val numberP: Parser[Snum] = P(CharIn("+-").? ~ CharIn('0' to '9').rep(1)).!.map(d => Snum(d.toInt))
    val symbolP: Parser[Ssym] = P(CharPred(!reserved_chars.contains(_)).rep(1).!).map(Ssym)
    val boolP: Parser[Sbool]  = P("#" ~/ CharIn("tf").!).map(s => if (s == "t") true else false).map(Sbool)
    val listP: Parser[Slist]  = P("(" ~/ (wspcP ~ sexpP).rep(0) ~ wspcP.? ~ ")").map(v => Slist(v.toList))
    val sexpP: Parser[Sexp]   = P(numberP | boolP | symbolP | listP)
    val quoteP: Parser[Sexp]  = P("quote" ~ sexpP) 
    
    //my project 
    //val set := Parser[Unit] = P ("(" ~ "set" ~ "'" ~ ident ~ (integer|atom|string) ~ ")")
    // supposed to work this way in lisp:
    // (set x 1) should give output: x = 1;
    // now x is assigned to 1; 
    //val string := Parser[Unit] = P("(" ~ "string" ~ (variable.rep | "'" ~ string.rep) ~ ")"))
    // supposed to work this way in lisp:
    // n1 = 1
    // n2 = 2
    // (string n1 n2)   should give output: 12;
    // (string 'n1 'n2) output: n1n2
    /*
    val sexp : Parser[Unit] = P( atom
    	| "(" ~ sexp.rep ~ ")" 
    	| "'" ~ sexp.rep
    	)
    val atom : Parser[Sexp] = P (integer | variable | ident | string)
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val variable : Parser[Sexp] = ident.map (s => Var (s))
    val string : Parser[Sexp] = P(String)
    val integer : Parser[Sexp] = digits.map (n => CstI (n))
	val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
	val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val keywords : List[String] = List ("let", "in", "end")
    */
    //val assign : Parser[String] = P (defvar)

    
    //val parens : Parser[Expr] = P (atom | ("(" ~ addSub ~ ")") | ("let" ~ ident ~ "=" ~ addSub ~ "in" ~ addSub ~ "end").map { case (x, erhs, ebody) => Let (x, erhs, ebody) })
    //val mult : Parser[Sexp] = P (parens ~ ("*".! ~ parens).rep.map (s => s.toList)).map (foldAssocLeft)
    //val addSub : Parser[Sexp] = P (mult ~ (("+" | "-").! ~ mult).rep.map (s => s.toList)).map (foldAssocLeft)
    //val start : Parser[Expr] = P (addSub ~ End)
    
    

    
    //val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    
    val start : Parser[Unit] = P (putYourStuffHere ~ End)
    val start1 : Parser[Unit] = P (quoteP ~ End)
    //val start2 : Parser[Unit] = P (string ~ End)
  }
  sealed trait Sexp
  case class Slist (l : List[Sexp])					          extends Sexp
  case class Snum (val value : Int)                   extends Sexp
  case class Ssym (val vlaue : String)                extends Sexp
  case class Sbool (val value: Boolean)

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Let (nm : String, e1 : Expr, e2 : Expr)  extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  import fastparse.all.{Parsed,Parser}

  def test (p : Parser[Unit], s : String) : Unit = {
    val result : fastparse.core.Parsed[Unit, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[Unit] = MyParsers.start
    val p02 : Parser[Unit] = MyParsers.start1
    //val p03 : Parser[Unit] = MyParsers.start2
    test (p01, "do some stuff here")

    /*
    (defun fatorial (n)
  		(if (= n 1) 
  			(1)							//if
  			(* n (fatorial (- n 1)))	//else
  		)
  	)
    */
    test (p02, "quote (Hello World))")
    //test (p03, "(string 'hell 'o)")

    println ("=" * 80)
  }
}
*/