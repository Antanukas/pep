package example.aoc

import java.util
import java.util.concurrent.{ArrayBlockingQueue, Executors, ThreadFactory}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.{Source, StdIn}
import scala.util.Random

object Day15 extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  //implicit val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  val originalProgram = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/program.in").getLines().next().split(",").map(BigInt(_))

  //runWithInputs()
  run()

  def run() = {
    val q1 = new ArrayBlockingQueue[BigInt](2, true)
    val q2 = new ArrayBlockingQueue[BigInt](2, true)

    val board = Array.fill(200, 200)(0)

    println("Lets play!!!")

    Future {
      runWithInputs(
        name = "robot",
        readInput = q1.take(),
        writeOutput = output => q2.put(output)
      )

      println("Game finished")
    }

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

