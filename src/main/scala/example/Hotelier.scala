package example

object Hotelier extends App {
  var N = 10

  var isInitiallyEmpty = true

  def solve(events: String): String = solve(events.split(""))

  def solve(events: Seq[String]): String = {
    import scala.collection.mutable
    val emptyRooms: mutable.TreeSet[Int] = if (isInitiallyEmpty)
      collection.mutable.TreeSet.from(0 until N)
    else collection.mutable.TreeSet()

    events.foreach {
      case "L" =>
        val firstEmpty = emptyRooms.min
        emptyRooms.remove(firstEmpty)
      case "R" =>
        val firstEmpty = emptyRooms.max
        emptyRooms.remove(firstEmpty)
      case n =>
        emptyRooms.add(Integer.parseInt(n))
    }
    val result = (0 until N).map(room => if (emptyRooms.contains(room)) "0" else "1").mkString
    result
  }

  assert(solve("LLRL1RL1") == "1010000011")
  assert(solve("L0L0LLRR9") == "1100000010")
  assert(solve("LLLLLLLLLL") == "1111111111")
  assert(solve("RRRRRRRRRR0123456789LLLLLRRRRR") == "1111111111")

  {
    N = 10000
    val input = scala.io.Source.fromFile("/Users/antanasb/Code/wix/pep/hotelier-large.txt").getLines().toSeq
    //warmup
    //(0 to 20).foreach(_ => solve(input))
    val startTime = System.currentTimeMillis()
    println("Large Start")
    solve(input)
    println(s"Solved in: ${System.currentTimeMillis() - startTime}ms")
  }

  {
    isInitiallyEmpty = false
    val input = scala.io.Source.fromFile("/Users/antanasb/Code/wix/pep/hotelier-mega.txt").getLines().toSeq
    val startTime = System.currentTimeMillis()
    println("Mega Start")
    solve(input)
    println(s"Solved in: ${System.currentTimeMillis() - startTime}ms")
  }
}

