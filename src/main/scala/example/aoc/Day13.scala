package example.aoc

import java.util.concurrent.{ArrayBlockingQueue, Executors, ThreadFactory}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.{Source, StdIn}

object Day13 extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool(new ThreadFactory {
    var c = 0
    val tf = Executors.defaultThreadFactory
    override def newThread(r: Runnable): Thread = {
      val t = tf.newThread(r)
      t.setName(s"my-thread-$c")
      t
    }
  }))
  //implicit val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  val originalProgram = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/program.in").getLines().next().split(",").map(BigInt(_))

  //runWithInputs()
  run()
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

  def printBoard(b: Array[Array[Int]], score: Int) = {
  //  Runtime.getRuntime.exec("cls")
    val transposed = b.transpose
    var i = 0
    println("Score:", score)
    while (i < transposed.length) {
      var j = 0
      while(j < transposed(i).length) {
        val simbol = transposed(i)(j) match {
          case 0 => " "
          case 1 => "#"
          case 2 => "."
          case 3 => "_"
          case 4 => "O"
        }
        print(simbol)
        j += 1
      }
      println()
      i += 1
    }
  }


  def run() = {
    val q1 = new ArrayBlockingQueue[BigInt](1, true)
    val q2 = new ArrayBlockingQueue[BigInt](10000, true)

   /* val f = Future {
      runWithInputs(
        name = "robot",
        readInput = q1.take(),
        writeOutput = output => q2.put(output)
      )
    }

    Await.result(f, Duration.Inf)
*/
/*    val grid = q2.toArray.map(_.toString.toInt)
    println(grid.max)
    println(grid.min)

    val board = Array.fill(43, 43)(0)
    var i = 0
    while (i < grid.length - 3) {
      val x = grid(i)
      val y = grid(i + 1)
      val tileId = grid(i + 2)
      board(x)(y) = tileId
      i += 3
    }*/

    //printBoard(board)

    val m = new Object
    var tile = 0
    val board = Array.fill(43, 21)(0)
    var score = 0
    q1.clear()
    val q3 = new ArrayBlockingQueue[BigInt](100, true)

    def findX = board
    /*q1.put(-1)
    (1 to 100).foreach(_ => q1.put(0))*/

    println("Lets play!!!")

    Future {
      runWithInputs(
        coin = Some(2),
        name = "robot",
        //readInput = q1.take(),
        readInput = {
          //m.synchronized(printBoard(board, score))
          //println()
          //0,0,0,1,1,1,1,1,1   -1,0,0,0,0,0,0,0,0,0
          //print("Input: ")
          //val i = StdIn.readLine().toInt
          //println()
         // printBoard(board, score)
          q1.take()
        },
        writeOutput = output => q3.put(output)
      )

      println("Game finished")
    }

    Future {
      while (true) { Thread.sleep(1000); println("Scoras", score)}
    }
//77, 84   (7
    Future {
      var isFinished = false
      try {
        while (!isFinished) {
          val x = q3.take().toInt
          val y = q3.take().toInt
          val tileId = q3.take().toInt
          if (x == -1 && y == 0) {
            score = tileId
            //println("Score: ", tileId)
            //isFinished = true
          } else {
            if (tileId == 3) tile = x
            if (tileId == 4) {
              val stick =  if (x == tile) 0 else if (x > tile) 1 else -1
              q1.offer(stick)
            }
            board(x)(y) = tileId
          }
        }
      } catch {
        case e => e.printStackTrace()
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
       //   println(s"[$name] Input: $input")
          program(address.toInt) = input
          pointer += 2
        case 4 =>
          val value = pIn(1)
          //println(s"[$name] Write output: $value")
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

/*
...............15
..........................26
..........................26
.........................25
......................22
.........................25
.........................25
......................22
................16
.....................21
.....................21
.................. 18
...................... 22

 */