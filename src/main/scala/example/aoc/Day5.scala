package example.aoc

import scala.io.{Source, StdIn}

object Day5 extends App {
  val originalProgram = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/program.in").getLines().next().split(",").map(_.toInt)

  runWithInputs()

  def runWithInputs(noun: Option[Int] = None, verb: Option[Int] = None): Int = {
    val program = originalProgram.clone()
    if (noun.isDefined) {
      program(1) = noun.get
      program(2) = verb.get
    }

    var pointer = 0
    var halted = false

    while(!halted) {
      val normalized = program(pointer).toString.prependedAll("00000").takeRight(5)
      val instruction = normalized.takeRight(2).toInt
      val parameterModes = normalized.take(3).toArray.map(n => Integer.parseInt(n.toString)).reverse

      //println(normalized, program.mkString(","))
      instruction match {
        case 1 =>
          val p1 = if (parameterModes(0) == 0) program(program(pointer + 1)) else program(pointer + 1)
          val p2 = if (parameterModes(1) == 0) program(program(pointer + 2)) else program(pointer + 2)
          val p3 = program(pointer + 3)
          val sum = p1 + p2
          program(p3) = sum
          pointer += 4
        case 2 =>
          val p1 = if (parameterModes(0) == 0) program(program(pointer + 1)) else program(pointer + 1)
          val p2 = if (parameterModes(1) == 0) program(program(pointer + 2)) else program(pointer + 2)
          val p3 = program(pointer + 3)
          val mul = p1 * p2
          program(p3) = mul
          pointer += 4
        case 3 =>
          val address = program(pointer + 1)
          println("Input:")
          val input = StdIn.readLine().toInt
          program(address) = input
          pointer += 2
        case 4 =>
          val address = program(pointer + 1)
          println(program(address))
          pointer += 2
        case 5 => //jump-if-true
          val p1 = if (parameterModes(0) == 0) program(program(pointer + 1)) else program(pointer + 1)
          val p2 = if (parameterModes(1) == 0) program(program(pointer + 2)) else program(pointer + 2)
          if (p1 > 0) pointer = p2 else pointer += 3
        case 6 => //jump-if-false
          val p1 = if (parameterModes(0) == 0) program(program(pointer + 1)) else program(pointer + 1)
          val p2 = if (parameterModes(1) == 0) program(program(pointer + 2)) else program(pointer + 2)
          if (p1 == 0) pointer = p2 else pointer += 3
        case 7 => //less-then
          val p1 = if (parameterModes(0) == 0) program(program(pointer + 1)) else program(pointer + 1)
          val p2 = if (parameterModes(1) == 0) program(program(pointer + 2)) else program(pointer + 2)
          val p3 = program(pointer + 3)
          program(p3) = if (p1 < p2) 1 else 0
          pointer += 4
        case 8 => //eq
          val p1 = if (parameterModes(0) == 0) program(program(pointer + 1)) else program(pointer + 1)
          val p2 = if (parameterModes(1) == 0) program(program(pointer + 2)) else program(pointer + 2)
          val p3 = program(pointer + 3)
          program(p3) = if (p1 == p2) 1 else 0
          pointer += 4
        case 99 => halted = true
      }
    }
    program(0)
  }



}
