package example.aoc

import java.util.concurrent.{ArrayBlockingQueue, Executors}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.{Source, StdIn}

object Day9 extends App {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  val originalProgram = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/program.in").getLines().next().split(",").map(BigInt(_))

  runWithInputs()
  /*val outputs = for {
    i1 <- 5 to 9
    i2 <- 5 to 9
    i3 <- 5 to 9
    i4 <- 5 to 9
    i5 <- 5 to 9
    if Set(i1, i2, i3, i4, i5).size == 5
  } yield run(Seq(i1, i2, i3, i4, i5))

  println("result", outputs.max)*/

  //println(run(Seq(9,7,8,5,6)))
  //println("it", runWithInputs())

  def run(inputs: Seq[Int]): BigInt = {
    val q1, q2, q3, q4, q5 = new ArrayBlockingQueue[BigInt](2, true)
    q1.put(inputs(0))
    q2.put(inputs(1))
    q3.put(inputs(2))
    q4.put(inputs(3))
    q5.put(inputs(4))

    q1.put(0) //starting input

    val futures: Seq[Future[(BigInt, Seq[BigInt])]] = Seq {
      Future {
        runWithInputs(
          name = "amp1",
          readInput = q1.take(),
          writeOutput = output => q2.put(output)
        )
      }

      Future {
        runWithInputs(
          name = "amp2",
          readInput = q2.take(),
          writeOutput = output => q3.put(output)
        )
      }

      Future {
        runWithInputs(
          name = "amp3",
          readInput = q3.take(),
          writeOutput = output => q4.put(output)
        )
      }

      Future {
        runWithInputs(
          name = "amp4",
          readInput = q4.take(),
          writeOutput = output => q5.put(output)
        )
      }

      Future {
        runWithInputs(
          name = "amp5",
          readInput = q5.take(),
          writeOutput = output => q1.put(output)
        )
      }
    }

    futures.foreach(f => Await.ready(f, Duration.Inf))

    q1.poll()
  }

  //50120
  def runWithInputs(
      name: String = "program",
      noun: Option[Int] = None,
      verb: Option[Int] = None,
      readInput: => BigInt = StdIn.readLine().toInt,
      writeOutput: BigInt => Unit = a => println(a)
  ): (BigInt, Seq[BigInt]) = {
    val program: Array[BigInt] = Array.fill(100000)(0)
    originalProgram.copyToArray(program)
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
          val sum = p1 +  p2
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
