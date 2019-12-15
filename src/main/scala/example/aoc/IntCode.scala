package example.aoc


import java.util.concurrent.{ArrayBlockingQueue, Executors}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn

class IntCode(originalProgram: Array[BigInt]) {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def runWithIOQueues(
      name: String = "program",
      noun: Option[Int] = None,
      verb: Option[Int] = None
  ): (ArrayBlockingQueue[BigInt], ArrayBlockingQueue[BigInt]) = {
    val in, out = new ArrayBlockingQueue[BigInt](2, true)
    Future {
      val res = runWithInputs(
        name = name,
        noun = noun,
        verb = verb,
        readInput = in.take(),
        writeOutput = output => out.put(output)
      )
      println(s"[$name] halted: ", res)
    }
    (in, out)
  }

  def runWithInputs(
      name: String = "program",
      noun: Option[Int] = None,
      verb: Option[Int] = None,
      readInput: => BigInt = {
        println()
        print("Input: ")
        StdIn.readLine().toInt
      },
      writeOutput: BigInt => Unit = a => println(a),
      coin: Option[Int] = None
  ): (BigInt, Seq[BigInt]) = {
    val program: Array[BigInt] = Array.fill(100000)(0)
    originalProgram.copyToArray(program)
    coin.foreach(c => program(0) = c)
    if (noun.isDefined) {
      program(1) = noun.get
      program(2) = verb.get
    }

    var pointer = 0
    var halted = false

    var relativeBase = 0
    var outputs = Seq.empty[BigInt]

    while (!halted) {
      val normalized = program(pointer).toString.prependedAll("00000").takeRight(5)
      val instruction = normalized.takeRight(2).toInt
      val parameterModes = normalized.take(3).toArray.map(n => Integer.parseInt(n.toString)).reverse

      def pIn(number: Int): BigInt = {
        if (parameterModes(number - 1) == 0) program(program(pointer + number).toInt)
        else if (parameterModes(number - 1) == 1) program(pointer + number)
        else program(program(pointer + number).toInt + relativeBase)
      }

      def pOut(number: Int): Int = {
        val offset = if (parameterModes(number - 1) == 2) relativeBase else 0
        program(pointer + number).toInt + offset
      }

      instruction match {
        case 1 =>
          val (p1, p2, p3) = (pIn(1), pIn(2), pOut(3))
          val sum = p1 + p2
          program(p3) = sum
          pointer += 4
        case 2 =>
          val (p1, p2, p3) = (pIn(1), pIn(2), pOut(3))
          val mul = p1 * p2
          program(p3) = mul
          pointer += 4
        case 3 =>
          val address = pOut(1)
          val input = readInput
          println(s"[$name] Input: $input")
          program(address.toInt) = input
          pointer += 2
        case 4 =>
          val value = pIn(1)
          println(s"[$name] Write output: $value")
          writeOutput(value)
          outputs = outputs :+ value
          pointer += 2
        case 5 => //jump-if-true
          val (p1, p2) = (pIn(1), pIn(2))
          if (p1 > 0) pointer = p2.toInt else pointer += 3
        case 6 => //jump-if-false
          val (p1, p2) = (pIn(1), pIn(2))
          if (p1 == 0) pointer = p2.toInt else pointer += 3
        case 7 => //less-then
          val p1 = pIn(1)
          val p2 = pIn(2)
          val p3 = pOut(3)
          program(p3) = if (p1 < p2) 1 else 0
          pointer += 4
        case 8 => //eq
          val (p1, p2, p3) = (pIn(1), pIn(2), pOut(3))
          program(p3) = if (p1 == p2) 1 else 0
          pointer += 4
        case 9 => //adjusts the relative base
          val p1 = pIn(1)
          relativeBase += p1.toInt
          pointer += 2
        case 99 => halted = true
      }
    }
    (program(0), outputs)
  }
}
