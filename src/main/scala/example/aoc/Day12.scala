package example.aoc

import scala.io.Source

object Day12 extends App {
  var orbits = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/input.in").getLines()

  case class Coord(var x: Long, var y: Long, var z: Long)

  case class Moon(pos: Coord, velocity: Coord) {
    val orig = pos.copy()
    def isOrig = pos == orig
    def applyVelocity() = {
      pos.x += velocity.x
      pos.y += velocity.y
      pos.z += velocity.z
    }

    def potentialEnergy = Math.abs(pos.x) + Math.abs(pos.y) + Math.abs(pos.z)
    def kineticEnergy = Math.abs(velocity.x) + Math.abs(velocity.y) + Math.abs(velocity.z)
  }

  var configurations = Map.empty[Seq[Coord], Int]
  def applyGravity(moons: Seq[Moon], delta: Int) = {
    var i = 0
    while (i < moons.size - 1) {
      var j = i + 1
      while (j < moons.size) {
        if (i != j) {
          val moon1 = moons(i)
          val moon2 = moons(j)
          val gx = Math.signum(moon1.pos.x - moon2.pos.x).toLong
          moon1.velocity.x -= gx * delta
          moon2.velocity.x += gx * delta

          val gy = Math.signum(moon1.pos.y - moon2.pos.y).toLong
          moon1.velocity.y -= gy * delta
          moon2.velocity.y += gy * delta

          val gz = Math.signum(moon1.pos.z - moon2.pos.z).toLong
          moon1.velocity.z -= gz * delta
          moon2.velocity.z += gz * delta
        }
        j += 1
      }
      i +=1
    }
  }

  val moons = Seq(
    Moon(Coord(4, 1, 1), Coord(0, 0, 0)),
    Moon(Coord(11, -18, -1), Coord(0, 0, 0)),
    Moon(Coord(-2, -10, -4), Coord(0, 0, 0)),
    Moon(Coord(-7, -2, 14), Coord(0, 0, 0))
  )

/*  val moons = Seq(
    Moon(Coord(-8, -10, 0), Coord(0, 0, 0)),
    Moon(Coord(5, 5, 10), Coord(0, 0, 0)),
    Moon(Coord(2, -7, 3), Coord(0, 0, 0)),
    Moon(Coord(9, -8, -3), Coord(0, 0, 0))
  )
  val moons = Seq(
    Moon(Coord(-1, 0, 2), Coord(0, 0, 0)),
    Moon(Coord(2, -10, -7), Coord(0, 0, 0)),
    Moon(Coord(4, -8, 8), Coord(0, 0, 0)),
    Moon(Coord(3, 5, -1), Coord(0, 0, 0))
  )*/

  var i = 0L
  var foundX, foundY, foundZ = false
  var lastMax = 0
  while (!(foundX && foundY && foundZ)) {

    if (i % 1000000 == 0) {
      println(s"Step $i Total energy:" , moons.map(m => m.kineticEnergy * m.potentialEnergy).sum)
      moons.foreach(println(_))
    }


    applyGravity(moons, delta = 1)
    moons.foreach(_.applyVelocity())

    val key = moons.map(_.pos.copy())
    val c = configurations.getOrElse(key, 0)
    configurations += key -> (c + 1)
    if ((c + 1) > lastMax) {
      lastMax = c + 1
      println("new max", lastMax)
    }

   // println()

    // 498000000
    //4686774924
    i += 1

    if (moons.forall(m => m.pos.x == m.orig.x) && !foundX) {
      println(s"X ${i + 1}")
      foundX = true
    }
    if (moons.forall(m => m.pos.y == m.orig.y) && !foundY) {
      println(s"Y ${i + 1}")
      foundY = true
    }
    if (moons.forall(m => m.pos.z == m.orig.z) && !foundZ) {
      println(s"Z ${i + 1}")
      foundZ = true
    }
    //then Online LCM calculator -> 326,365,108,375,488


  }

  //Y 84030
  //Z 231612
  //X 268294
  // 435,136,119,770,820

  //2026 5896 4700
}
