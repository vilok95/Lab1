package jsy.student

import jsy.lab1._

import scala.collection.immutable.Stream

object Lab1 extends jsy.util.JsyApplication with jsy.lab1.Lab1Like {
  import jsy.lab1.Parser
  import jsy.lab1.ast._

  /*
   * CSCI 3155: Lab 1
   * Vilok Krishnan
   *
   * Partner: Val
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
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
   * '???' as needed to get something that compiles without error.
   */

  /*
   * Example: Test-driven development of plus
   *
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter. The simplest way
   * to use the interactive Scala interpreter in IntelliJ is through a worksheet,
   * such as Lab1Worksheet.sc. A Scala Worksheet (e.g., Lab1Worksheet.sc) is code
   * evaluated in the context of the project with results for each expression
   * shown inline.
   *
   * Step 0: Sketch an implementation in Lab1.scala using ??? for unimmplemented things.
   * Step 1: Do some experimentation in Lab1Worksheet.sc.
   * Step 2: Write a test in Lab1Spec.scala, which should initially fail because of the ???.
   * Step 3: Fill in the ??? to finish the implementation to make your test pass.
   */

  //def plus(x: Int, y: Int): Int = ???
  def plus(x: Int, y: Int): Int = x + y

  /* Exercises */

  def abs(n: Double): Double = {
    if (n > 0) {
      n
    }
    else {
      -n
    }
  }


  def xor(a: Boolean, b: Boolean): Boolean = {
    if (a == b){
      false
    }
    else{
      true
    }
  }

  def repeat(s: String, n: Int): String = {
    require(n>(-1))
    if (n < 1){
      ""
    }
    else{
      s + repeat(s, n-1)
    }
  }

  def sqrtStep(c: Double, xn: Double): Double = {
    xn - (((xn*xn)-c)/(2*xn))
  }

  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    require(n>(-1))
    n match {
      case 0 => x0
      case _ =>sqrtN(c,sqrtStep(c,x0),n-1)

    }
  }



  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    require(epsilon >(0))
    if (abs((x0*x0)-c)>epsilon){
      sqrtErr(c,sqrtStep(c,x0),epsilon)
    }
    else x0
  }
  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }

  /* Search Tree */

  // Defined in Lab1Like.scala:
  //
  // sealed abstract class SearchTree
  // case object Empty extends SearchTree
  // case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree

  def repOk(t: SearchTree): Boolean = {
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      case Empty => true
      case Node(l, d, r) => {
        if (d < min || d > max){
          false
        }
        else {
          check(l,min,d) && check(r,d,max)
        }
      }
    }
    check(t, Int.MinValue, Int.MaxValue)
  }

  def insert(t: SearchTree, n: Int): SearchTree = { t match {
    case Empty => Node(Empty,n,Empty)
    case Node(l,d,r) => {
      if (n<d){
        Node(insert(l,n),d,r)
      }
      else {
        Node(l,d,insert(r,n))
      }
    }
  }

  }

  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d)
      case Node(l, d, r) =>
        val (l1, m) = deleteMin(l)
        l1 match {
          case Empty => (Node(Empty,d,r),m)
        }
    }
  }

  def delete(t: SearchTree, n: Int): SearchTree = t match {
    case Empty => Empty
    case Node(Empty,d,Empty) => if (d==n) {Empty} else Node(Empty,d,Empty)
    case Node(Empty,d,r) => if (n>d) delete(r,n) else Node(Empty,d,r)
    case Node(l,d,Empty) => if (n<d) delete(l,n) else Node(l,d,Empty)
    case Node(l,d,r) => {
      if (n>d) delete(r,n)
      else if (n<d) delete(l,n)
      else (n==d)

  }

  /* JavaScripty */

  def eval(e: Expr): Double = e match {
    case N(n) => n
    case Unary(uop,e1) => uop match {
      case Neg => -eval(e1)
    }
    case Binary(bop,e1,e2) => bop match{
      case Plus => eval(e1) + eval(e2)
      case Minus => eval(e1) - eval(e2)
      case Times => eval(e1) * eval(e2)
      case Div => eval(e1) / eval(e2)
    }
  }

 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */

 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(prettyNumber(v))
  }

}
