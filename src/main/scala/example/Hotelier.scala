package example

object Hotelier extends App {
  def solve10(events: String): String = {
    class Room(var state: Int)
    val rooms = (0 to 9).map(_ => new Room(0))

    events.foreach {
      case 'L' => rooms.collectFirst { case room if room.state == 0 => room }.foreach(_.state = 1)
      case 'R' => rooms.reverse.collectFirst { case room if room.state == 0 => room }.foreach(_.state = 1)
      case n => rooms(Character.getNumericValue(n)).state = 0
    }
    rooms.map(_.state).mkString
  }

  assert(solve10("LLRL1RL1") == "1010000011")
  assert(solve10("LLLLLLLLLL") == "1111111111")
  assert(solve10("RRRRRRRRRR0123456789LLLLLRRRRR") == "1111111111")
}

