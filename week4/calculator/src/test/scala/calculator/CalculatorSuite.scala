package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("computeDelta => x^2 + 5x + 6 = 0") {
    val a = Var(1.0)
    val b = Var(5.0)
    val c = Var(6.0)
    val result = Polynomial.computeDelta(a, b, c)
    assert(result() === 1)
  }

  test("computeDelta => 5x² + 6x + 1 = 0") {
    val a = Var(5.0)
    val b = Var(6.0)
    val c = Var(1.0)
    val result = Polynomial.computeDelta(a, b, c)
    assert(result() === 16)
  }

  test("computeSolutions => x^2 + 5x + 6 = 0") {
    val a = Var(1.0)
    val b = Var(5.0)
    val c = Var(6.0)
    val delta = Polynomial.computeDelta(a, b, c)
    val result = Polynomial.computeSolutions(a, b, c, delta)
    assert(result() === Set(-3.0, -2.0))
  }

  test("computeSolutions => 5x² + 6x + 1 = 0") {
    val a = Var(5.0)
    val b = Var(6.0)
    val c = Var(1.0)
    val delta = Polynomial.computeDelta(a, b, c)
    val result = Polynomial.computeSolutions(a, b, c, delta)
    assert(result() === Set(-0.2, -1.0))
  }

  test("eval Literal") {
    val expr = Literal(1.0)
    assert(Calculator.eval(expr, null) === 1.0)
  }

  test("eval Ref") {
    val references: Map[String, Signal[Expr]] = Map("b" -> Var(Literal(1.0)))
    val refExpr = Ref("b")
    assert(Calculator.eval(refExpr, references) === 1.0)
  }

  test("cyclic Ref") {
    val refA = Ref("a")
    val refB = Ref("b")
    val references: Map[String, Signal[Expr]] =
      Map("a" -> Var(refB), "b" -> Var(refA))
    assert(Calculator.eval(refA, references) equals Double.NaN)
  }

  test("eval Plus") {
    val expr1 = Literal(1.0)
    val expr2 = Literal(2.0)
    val plusExpr = Plus(expr1, expr2)
    assert(Calculator.eval(plusExpr, null) === 3.0)
  }

  test("eval Minus") {
    val expr1 = Literal(1.0)
    val expr2 = Literal(2.0)
    val minusExpr = Minus(expr1, expr2)
    assert(Calculator.eval(minusExpr, null) === -1.0)
  }

  test("eval Times") {
    val expr1 = Literal(1.0)
    val expr2 = Literal(2.0)
    val timesExpr = Times(expr1, expr2)
    assert(Calculator.eval(timesExpr, null) === 2.0)
  }

  test("eval Divide") {
    val expr1 = Literal(1.0)
    val expr2 = Literal(2.0)
    val divideExpr = Divide(expr1, expr2)
    assert(Calculator.eval(divideExpr, null) === 0.5)
  }
}
