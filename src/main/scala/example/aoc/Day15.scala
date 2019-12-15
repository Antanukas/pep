package example.aoc

import java.util
import java.util.concurrent.Executors

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

object Day15 extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  val originalProgram = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/program.in").getLines().next().split(",").map(BigInt(_))

  val intCode = new IntCode(originalProgram)
  //runWithInputs()
  run()

  def run() = {

    val board = Array.fill(200, 200)(0)

    println("Lets play!!!")

    val (q1, q2) = intCode.runWithIOQueues(name = "robot")

    Future {
      def move(x: Int, y: Int, direction: Int): (Int, Int) =  direction match {
        case 1 => (x, y - 1)
        case 2 => (x, y + 1)
        case 3 => (x - 1, y)
        case 4 => (x + 1, y)
      }

      def back(direction: Int) = direction match {
        case 1 => 2
        case 2 => 1
        case 3 => 4
        case 4 => 3
      }

      var x, y = 100
      var path = Set((x, y))
      var emptyCells = Set((x, y))

      def singleMove(d: Int): Int = {
        q1.put(d)
        val result = q2.take().toInt
        if (result == 0) {
          val (nx, ny) = move(x, y, d)
          board(nx)(ny) = 1 //wall
          result
        } else if (result == 1 || result == 2) {
          val (nx, ny) = move(x, y, d)
          path = path.+((nx, ny))
          emptyCells = emptyCells + ((nx, ny))
          board(x)(y) = 0
          board(nx)(ny) = 2
          x = nx
          y = ny
          result
        } else result
      }

      var shortestPath = Int.MaxValue
      try {
        board(x)(y) = 2
        var found = false
        val stack = new util.Stack[Int]()
        /*var direction = Random.nextInt(4) + 1
        stack.push(direction)*/

        val visited = mutable.Set.empty[Set[(Int, Int)]]
        //visited.add((direction, x, y))

        def moveBack() = {
          //if (!stack.empty()) {
            val lastMove = stack.pop()
            path = path.-((x, y))
            singleMove(back(lastMove))
          //} else Random.nextInt(4) + 1
        }

        def printBoard(b: Array[Array[Int]]) = {
          //  Runtime.getRuntime.exec("cls")
          val transposed = b
          var i = 0
          while (i < transposed.length) {
            var j = 0
            while(j < transposed(i).length) {
              val simbol = transposed(i)(j) match {
                case _ if (i, j) == (100, 100) => "X"
                case 0 => if (path.contains(i, j)) "." else " "
                case 1 => "#"
                case 2 => "O"
              }
              print(simbol)
              j += 1
            }
            println()
            i += 1
          }
        }

        val allMoves = Seq(1, 2, 3, 4).reverse

        try {
          while (!found) {
            println("shortestPath", shortestPath)
            // printBoard(board)

            val nextMove = allMoves.reverse.collectFirst {
              case d if !visited.contains(Set(move(x, y, d))) => (d, move(x, y, d)._1, move(x, y, d)._2)
            }
            //val nextMove = Seq(1, 2, 3, 4).map(d => (d, move(x, y, d)._1,  move(x, y, d)._2)).find(next => !visited.contains(next))
            if (nextMove.isDefined) {
              val (direction, nx, ny) = nextMove.get
              val result = singleMove(direction)
              visited.add(Set((nx, ny)))
              if (result != 0) {
                stack.push(direction)
              }

              /*if (result == 2) {
                if (shortestPath > path.size) shortestPath = path.size
                println("FOUND OXYGEN", x, y, path.size, shortestPath)
                found = true
              }*/
            } else {
              moveBack()
            }
          }
        } catch {
          case e => e.printStackTrace()
        }

        printBoard(board)
        val startX, startY = 100
        val targetX, targetY = 114

        case class Node(x: Int, y: Int, depth: Int)

        println(emptyCells)
        def buildTree(x: Int, y: Int, depth: Int, visited: Set[(Int, Int)]): Seq[Node] = {
          val n = Node(x, y, depth)
          Seq(n) ++ allMoves.map(d => move(x, y, d)).filter(emptyCells.contains(_))
              .filter(coord => !visited.contains(coord))
            .flatMap(coord => buildTree(coord._1, coord._2, depth + 1, visited + coord))
        }

        //357
        println(buildTree(targetX, targetY, 0, Set.empty)
          .maxBy(_.depth))

      } catch {
        case e =>
          println("shortestPath", shortestPath)
          e.printStackTrace()
      }
    }
  }
}

