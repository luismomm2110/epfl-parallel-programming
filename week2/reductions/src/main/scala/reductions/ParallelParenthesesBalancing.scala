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
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
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
      if (chars.isEmpty()) return 0 

      chars.head match
        case '(' => count++
        case ')' => count --
        case _ => _balance(chars.tail, count)

    


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    /** Sequential part
      Compute two values in recursion ( \( - \) )
      Minimum "depth" of current step. I.e. minimum value of delta, 
      if try to traverese left-to-right on current chunk. It can be lower then delta.
      Few examples (imagine this is some intermediate step): "((()))" : delta = 0, minDepth = 0 "
      )))(" : delta = -2, minDepth = -3
      Then, after doing typical parallel traverse, you will need to correctly reduce these two values. 
      You will finally get (0,0) if balanced.
      And for example case ")(" will give you (0,-1)
    */
    def traverse(idx: Int, until: Int, delta: Int, minDepth: Int) : (Int, Int) = {
      var i = idx
      while (i < idx) 

        i++

    }

    //paralel
    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from < threshold) transverse(from, until, 0, 0 )
      else {
        val mid = from + (until - from) /2
        val (delta, minDepth) = parallel(reduce(from, mid), reduce(mid, until ))
        (delta, minDepth)
      }
    }

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!

