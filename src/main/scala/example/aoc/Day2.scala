package example.aoc

import scala.io.Source

object Day2 extends App {
  val originalProgram = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/program.in").getLines().next().split(",").map(_.toInt)

  println {
    for {
      noun <- 1 to 99
      verb <- 1 to 99
      if runWithInputs(noun, verb) == 19690720
    } yield (noun, verb)
  }

  def runWithInputs(noun: Int, verb: Int): Int = {
    val program = originalProgram.clone()
    program(1) = noun
    program(2) = verb

    var pointer = 0
    var halted = false

    while(!halted) {
      val instruction = program(pointer)
      instruction match {
        case 1 =>
          val sum = program(program(pointer + 1)) + program(program(pointer + 2))
          program(program(pointer + 3)) = sum
          pointer += 4
        case 2 =>
          val mul = program(program(pointer + 1)) * program(program(pointer + 2))
          program(program(pointer + 3)) = mul
          pointer += 4
        case 99 => halted = true
      }
    }
    program(0)
  }



}
