package example.aoc

import java.util.concurrent.Executors

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.io.Source

object Day17 extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  val originalProgram = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/program.in").getLines().next().split(",").map(BigInt(_))

  val intCode = new IntCode(originalProgram)
  //runWithInputs()

  def toChar(v: Int): Char = if (v == 10) '\n' else v.toChar

  calc
  def calc = {
    val coord = Seq(
      13, 20,
      13, 32,
      13, 38,
      15, 30,
      15, 32,
      19, 20,
      19, 22,
      21, 22,
      21, 28,
      21, 30,
      25, 36,

    )
/*
    val coord = Seq(
      3,4,
    5,4,
    5,8,
    5,12,
    )*/
    var i = 0
    var sum = 0
    while (i < coord.size) {
      val y = coord(i)
      val x = coord(i + 1)

      sum += (x - 2) * (y - 1)
      i += 2
    }
    println("sum", sum)
  }

  var board = Array.fill(51, 45)('.')

  var robotX, robotY = 0

  {
    var x, y = 1
    intCode.runWithInputs(writeOutput = output => {
      output.toChar match {
        case '^' =>
          robotX = x
          robotY = y
        case '>' =>
          robotX = x
          robotY = y
        case '<' =>
          robotX = x
          robotY = y
        case 'v' =>
          robotX = x
          robotY = y
        case _ => ()
      }
      if (output.toInt == 10) {
        y += 1
        x = 1
      } else {
        board(x)(y) = toChar(output.toInt)
        x += 1
      }
      print(toChar(output.toInt))
    })
  }

   var finished = false

  def toDirection(d: (Int, Int)) = {
    d match {
      case (0, -1) => '^'
      case (1, 0) => '>'
      case (-1, 0) => '<'
      case (0, 1) => 'v'
    }
  }

  def fromDirection(d: Char): (Int, Int) = d match {
    case '^' => (0, -1)
    case '>' => (1, 0)
    case '<' => (-1, 0)
    case 'v' => (0, 1)
  }

  def left(): Option[(Char, (Int, Int))] = {
    val dir = board(robotX)(robotY) match {
      case '^' => '<'
      case '>' => '^'
      case '<' => 'v'
      case 'v' => '>'
    }

    val (newX, newY) = (robotX + fromDirection(dir)._1, robotY + fromDirection(dir)._2)
    if (board(newX)(newY) == '#' || board(newX)(newY) == 'Z') Some(('L', fromDirection(dir))) else None
  }

  def right(): Option[(Char, (Int, Int))] = {
    val dir = board(robotX)(robotY) match {
      case '^' => '>'
      case '>' => 'v'
      case '<' => '^'
      case 'v' => '<'
    }

    val (newX, newY) = (robotX + fromDirection(dir)._1, robotY + fromDirection(dir)._2)
    if (board(newX)(newY) == '#' || board(newX)(newY) == 'Z') Some(('R', fromDirection(dir))) else None
  }

  def pp() = {
    var i = 0
    val b = board.transpose
    while (i < b.length) {
      var j = 0
      while (j < b(i).length) {
        print(b(i)(j))
        j += 1
      }
      println
      i += 1
    }
  }
  println("Calculate moves", robotX, robotY, board(robotX)(robotY), board(robotX + 1)(robotY), board(robotX - 1)(robotY))
  val moves = mutable.Buffer.empty[String]
  while (!finished) {
   pp()
   // Thread.sleep(200)
    println(left().orElse(right()))
    left().orElse(right()) match {
      case Some((direction, newDir)) =>
        var i = 0

        var nextX = robotX + newDir._1
        var nextY = robotY + newDir._2
        while(board(nextX)(nextY) == '#' || board(nextX)(nextY) == 'Z') {
          board(robotX)(robotY) = 'Z'
          robotX = nextX
          robotY  = nextY
          println(robotX, robotY)
          board(robotX)(robotY) = toDirection(newDir)
          nextX = robotX + newDir._1
          nextY = robotY + newDir._2
          i += 1
        }
        println(direction.toString, i)
        moves += direction.toString
        moves += i.toString
      case None => finished = true
    }
  }

  println(moves.mkString(","))


  val i = Seq(
    "A,C,A,B,C,A,B,C,A,B".toCharArray,
    Array(10.toChar),
    "L,12,L,12,L,6,L,6".toCharArray,
    Array(10.toChar),
    "L,12,L,6,R,12,R,8".toCharArray,
    Array(10.toChar),
    "R,8,R,4,L,12".toCharArray,
    Array(10.toChar),
    "n".toCharArray,
    Array(10.toChar),
  )
  val instructions = i.flatten.iterator

  println(i.flatten.map(_.toInt).mkString(","))


  var sum = 0
  intCode.runWithInputs("robot", zeroAddress = Some(2), readInput = instructions.next().toInt,
    writeOutput = output => println(output)

      //sum += output.toInt
  )
  println("sum", sum)
}

/*
A L,12,L,12,L,6,L,6

CABABAB



 */
