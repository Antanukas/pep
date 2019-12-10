package example.aoc

import com.google.common.math.DoubleMath

import scala.collection.mutable
import scala.io.Source

object Day102 extends App {
  var map = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/input.in").getLines()

  val asteroids = mutable.Set.empty[(Int, Int)]


  var height: Int = 0
  var length: Int = 0
  while (map.hasNext) {
    val row = map.next().split("").iterator
    var x = 0
    while (row.hasNext) {
      val point = row.next()
      if (point == "#") {
        asteroids.addOne((x, height))
      }
      x += 1
    }
    length = x
    height += 1
  }

  def dist(start: (Int, Int), end: (Int, Int)): Double = {
    val (px, py) = start
    val (qx, qy) = end

    Math.sqrt(Math.pow(px - qx, 2) + Math.pow(py - qy, 2))
  }

  def isBetween(start: (Int, Int), end: (Int, Int))(candidate: (Int, Int)): Boolean = {
    val (ax, ay) = start
    val (bx, by) = end
    val a = by - ay
    val b = ax - bx
    val c = a * ax + b * ay

    val (x, y) = candidate
    val result = a * x + b * y == c
    println("res", start, end, candidate, result)
    result
  }

  def gcd(x: Long, y: Long): Long = if (y == 0) x else gcd(y, x % y)

  def isOnLine(start: (Int, Int), end: (Int, Int), candidate: (Int, Int)): Boolean  = {
    val (ax, ay) = start
    val (bx, by) = end
    val a = by - ay
    val b = ax - bx
    val c = a * ax + b * ay

    val (x, y) = candidate
    val result = a * x + b * y == c
    result
  }

  def slope(start: (Int, Int), end: (Int, Int)): Double = {
    val (ax, ay) = start
    val (bx, by) = end
    if (bx - ax == 0) Double.MaxValue else (by - ay) / (bx - ax)
  }

  def isBetween2(start: (Int, Int), end: (Int, Int))(candidate: (Int, Int)): Boolean = {
    val (ax, ay) = start
    val (cx, cy) = end
    val (bx, by) = candidate

    def isBetween(a: Int, b: Int, c: Int) = Array(a, b, c).sorted.apply(1) == b

    val res = if (ax == cx)
      bx == cx && isBetween(ay, by, cy)
    else if (ay == cy)
      by == cy && isBetween(ax, bx, cx)
    else {
      DoubleMath.fuzzyEquals(dist(start, end), dist(start, candidate) + dist(candidate, end), 0.000000001)
    }


    res
  }

  val laserPoint = (19, 11)
  var visibility = Map.empty[(Int, Int), Int]
  asteroids.foreach { asteroid =>


    val visible = asteroids.filter(_ != asteroid).filter { candidate =>
      val good = !asteroids.filter(other => candidate != other && asteroid != other)
          .exists(isBetween2(asteroid, candidate))
      good
    }

    /*
.X..Y
.....
YX1YX
....X
...CX
     */
    //println("seenPoints", seenPoints)
    visibility += asteroid -> visible.size
  }

  println(asteroids)
  pp(visibility)
  println(visibility.values.max)
  println("coordinates", visibility.filter { case (k, v) => v == 230 })

  def pp(v: Map[(Int, Int), Int]) = {
    var y = 0
    while (y < height) {
      var x = 0
      while (x < length) {
        print(v.get((x, y)).map(_.toString).getOrElse("."))
        x += 1
      }
      println
      y += 1
    }
  }

}

