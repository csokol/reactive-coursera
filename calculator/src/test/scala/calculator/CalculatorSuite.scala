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


  test("should eval literal") {
    val map: Map[String, Signal[Expr]] = Map()
    assert(Calculator.eval(Literal(10.0), map) == 10.0)
  }

  test("should eval ref") {
    val map: Map[String, Signal[Expr]] = Map("bla"->Var(Literal(5)))
    assert(Calculator.eval(Ref("bla"), map) == 5)
  }

  test("should eval simple sum") {
    val map: Map[String, Signal[Expr]] = Map("bla"->Var(Literal(5)))
    assert(Calculator.eval(Plus(Literal(1), Literal(2)), map) == 3)
  }

  test("should prevent infinte loop") {
    val a = Plus(Ref("b"), Literal(1))
    val b = Plus(Ref("a"), Literal(2))
    val map: Map[String, Signal[Expr]] = Map(("a"->Var(a)), ("b"->Var(b)))
    assert(Calculator.eval(Ref("a"), map).isNaN)
  }

  test("should compute one") {
    val map: Map[String, Signal[Expr]] = Map("bla"->Var(Literal(5)))
    val value = Calculator.computeValues(map)("bla")
    assert(value() == 5.0)
  }

  test("should compute twice") {
    val map: Map[String, Signal[Expr]] = Map(
      "bla"->Var(Literal(5)),
      "x"->Var(Ref("bla")),
      "y"->Var(Plus(Ref("bla"), Literal(2)))
    )
    val value = Calculator.computeValues(map)("y")
    assert(value() == 7.0)
  }

}
