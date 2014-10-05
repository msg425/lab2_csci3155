object Lab2 extends jsy.util.JsyApplication {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   */

  /*
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */

  /* We represent a variable environment is as a map from a string of the
   * variable name to the value to which it is bound.
   *
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */

  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }

  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case _ => throw new UnsupportedOperationException
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case _ => throw new UnsupportedOperationException
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case _ => throw new UnsupportedOperationException
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */
      case N(n) => e
      case B(b) => e
      case S(s) => e
      case Undefined => e

      /* Inductive Cases */
      case Print(e1) => println(pretty(eToVal(e1))); Undefined

      case ConstDecl(x, e1, e2) => e1

      case Unary(op, e) => op match {
        case Neg => N( -1 * toNumber( eval(e) ) )
        case Not => B( !toBoolean( eval(e) ) )
      }
      case Binary(op, e1, e2) => op match{
        /* + */
        case Plus => N(toNumber(eToVal(e1)) + toNumber(eToVal(e2)))
        /* - */
        case Minus => N(toNumber(eToVal(e1)) - toNumber(eToVal(e2)))
        /* * */
        case Times => N(toNumber(eToVal(e1)) * toNumber(eToVal(e2)))
         /* / */
        case Div => e1
        /* === */
        case Eq => e1
        /* !=== */
        case Ne => e1
        /* < */
        case Lt => e1
        /* <= */
        case Le => e1
        /* > */
        case Gt => e1
        /* >= */
        case Ge => e1
        /* && */
        case And => {
          val b1 = eToVal(e1)
          val b2 = eToVal(e2)
          if ( toBoolean(b2) ) b1 else b2
        }
        /* || */
        case Or => {
          val b1 = eToVal(e1)
          val b2 = eToVal(e2)
          if ( toBoolean(b2) ) b2 else b1
        }
        /* , */
        case Seq => e1
      }

      case If(e1, e2, e3) => e1

      case _ => throw new UnsupportedOperationException
    }
  }

  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = eval(Parser.parse(s))

 /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(pretty(v))
  }

}
