package example.aoc

import java.util.concurrent.Executors

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.io.Source

object Day19 extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  val originalProgram = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/program.in").getLines().next().split(",").map(BigInt(_))

  val intCode = new IntCode(originalProgram)

  val startI, startJ = 200
  val n, m = 100

  {
    //val (q1, q2) = intCode.runWithIOQueues()
    var i = 1000
    var j = 0

    var found = false
    while (!found) {

      var foundJ = false
      var sum = 0
      var toggle = false
      while (!foundJ) {
       // q1.put(BigInt(i))
       // q1.put(BigInt(j))

        val input = Seq(i, j).iterator
        val (_, List(v)) = intCode.runWithInputs(readInput = input.next())
        if (v == 0 && toggle) {
          //TODO off by one
          val input2 = Seq(i, j - 100).iterator
          val (_, List(v1)) = intCode.runWithInputs(readInput = input2.next())
          if (v1 == 1) {
            //10140557 wrong
            val input3 = Seq(i + 99, j - 100).iterator
            val (_, List(v2)) = intCode.runWithInputs(readInput = input3.next())
            if (v2 == 1) {
              println("found", i * 10000 + (j - 100))
              found = true
            }
          }
          foundJ = true
        }
        if (v == 1) {
          toggle = true
        }
        sum += v.toInt
        //print(if (v == 1) '#' else '.')
        j+= 1
      }
      j = j - sum - 1

      if (sum > 98) {
        println("line", i, j, sum, i * 10000 + j)
      }
/*      if (sum == 105) {
        found = true
      }*/
      //676,338
      i +=1
    }

  }

  intCode
}

