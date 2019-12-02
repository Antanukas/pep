package example.aoc

import scala.io.Source

object Day1 extends App {
  val masses = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/input.in").getLines().map(_.toLong)

  println(masses.map { m =>
    var fuel = (m / 3) - 2
    var sum = 0L
    while (fuel > 0) {
      sum += fuel
      fuel = (fuel / 3) - 2
    }
    sum
  }.sum)
}
