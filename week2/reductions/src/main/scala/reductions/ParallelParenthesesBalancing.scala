package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 80000
    val chars = new Array[Char](length)
    val threshold = 8000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = 
    def _balance(chars: Array[Char], count: Int) : Int =
      if (chars.isEmpty || count < 0) return count
      
      chars.head match
        case '(' => _balance(chars.tail, count+1)
        case ')' => _balance(chars.tail, count-1)
        case _ => _balance(chars.tail, count)
      
    _balance(chars, 0) == 0


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    /** Sequential part
    */
    def traverse(idx: Int, until: Int, totalGain: Int, minDepth: Int) : (Int, Int) = {
      var i = idx
      var gain = 0 
      var depth = 0
      while (i < until) 
        chars(i) match
          case '(' => {
            gain = gain + 1
          } 
          case ')' => {
            gain = gain - 1
            depth = depth - 1
          } 
          case _ => 
        i = i + 1

      (totalGain + gain, depth+minDepth)
    }

    //paralel
    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0 )
      else {
        val mid = from + (until - from) /2
        val (resultL, resultR) = parallel(reduce(from, mid), reduce(mid, until))
        val soma = (resultL._1 + resultR._1)
        val nest = (resultL._2.min(resultR._2 + resultL._1)) /// min entre minnest da esquerda e soma tot gain da esq + min nest da direita
        // 
        (soma, nest) // ver como desconstruir tupla
      }
    }

    /*
      ([1, 0] ) [-1, -1]

    */

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!
