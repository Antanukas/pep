package example.aoc

import scala.io.Source

object Day3 extends App {

  var wires = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/input.in").getLines()
  var wire1 = wires.next().split(",")
  var wire2 = wires.next().split(",")

  val wire1Steps = pathToCoordinates(wire1)
  val wire2Steps = pathToCoordinates(wire2)

  val cross = wire1Steps.keySet.intersect(wire2Steps.keySet)
  val distance = cross.map(point => wire1Steps(point) + wire2Steps(point)).min

  println(distance)

  case class Point(x: Int, y: Int) {
    def manhattanDistance = Math.abs(x) + Math.abs(y)
  }

  def pathToCoordinates(path: Array[String]): Map[Point, Int] = {
    var currentX = 0
    var currentY = 0
    var currentStep = 0
    var stepMap = Map.empty[Point, Int]

    for {
      move <- path.toSeq

      direction = move.charAt(0)
      steps = move.substring(1).toInt

      _ <- 1 to steps
    } yield {
      currentStep += 1
      direction match {
        case 'R' => currentX += 1
        case 'L' => currentX -= 1
        case 'U' => currentY += 1
        case 'D' => currentY -= 1
      }
      val point = Point(currentX, currentY)
      stepMap = stepMap.updated(point, currentStep)
      point
    }

    stepMap
  }
}
