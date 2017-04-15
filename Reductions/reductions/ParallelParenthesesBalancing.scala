package reductions

import scala.annotation._
import org.scalameter._
import common._
import Math._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def leftBracketCounter(openLeftBrackets: Int, currentPosition :Int): Int = {
      if (currentPosition == chars.length) openLeftBrackets
      else {
        val nextChar = chars(currentPosition)
        if  (nextChar == '(' ) leftBracketCounter(openLeftBrackets + 1, currentPosition + 1)
        else if (nextChar == ')' && openLeftBrackets == 0) -1
        else if (nextChar == ')' && openLeftBrackets > 0) leftBracketCounter(openLeftBrackets - 1, currentPosition + 1)
        else leftBracketCounter(openLeftBrackets, currentPosition + 1)
      }
    }
    leftBracketCounter(0, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, min: Int, change: Int): (Int, Int) = {
      var localMin = min
      var netChange = change
      var i = idx
      while (i < until) {
        if (chars(i) == '(') netChange += 1
        else if (chars(i) == ')') {
          if (netChange <= 0) localMin -= 1
          netChange -= 1
        }
        i+=1
      }
      (localMin, netChange)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))
        val overallMin = min(left._1, left._2 + right._1)
        val overallChange = left._2 + right._2
        (overallMin, overallChange)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
