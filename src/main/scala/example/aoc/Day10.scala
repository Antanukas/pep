package example.aoc

import com.google.common.math.DoubleMath

import scala.collection.mutable
import scala.io.Source

object Day10 extends App {
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

  def isBetween2(start: (Int, Int), end: (Int, Int))(candidate: (Int, Int)): Boolean = {
    val (ax, ay) = start
    val (bx, by) = end
    val a = by - ay
    val b = ax - bx
    val c = a * ax + b * ay

    val (x, y) = candidate
    val result = a * x + b * y == c
    result
  }

  def isHiddenBy(q: (Int, Int), p: (Int, Int)): Boolean = {
    val (qx, qy) = q
    val (px, py) = p
    val tangent = if (qx - px == 0) Double.MaxValue else Math.tan((qy - py) / (qx - px))
    println("aaa", tangent, q, p)
    px == qx || py == qy || Math.abs(tangent) == 1
  }

  var visibility = Map.empty[(Int, Int), Int]
  val monitoring = (19,11)
  var killCount = 0
  def incKillCount(asteroid: (Int, Int)) = {
    killCount += 1
    asteroids.remove(asteroid)
    println(s"killed $killCount: ", asteroid)
  }

  case class Asteroid(x: Int, y: Int, angle: Double, dist: Double) {
    def coord = (x, y)
  }
  def angleNorm(angle: Double) = angle //if (angle <= 0) angle * -2 else angle
  val byangleAndDist = asteroids.filter(_ != monitoring).map { case (x, y) => Asteroid(x, y, -Math.atan2(x - monitoring._1, y - monitoring._2), dist(monitoring, (x, y))) }.toSeq
  val allAngles = byangleAndDist.map(_.angle).distinct.sorted

  println(allAngles)
  val killed = mutable.Set[Asteroid]()
  var i = 0
  var j = 0
  while (j < 300) {
    val angle = allAngles(i)

    //is visibile
    val allAlive = byangleAndDist.filter(_.angle == angle).filter(a => !killed.contains(a))
    val visible = allAlive.filter(_.coord != monitoring).filter { candidate =>
      !allAlive.filter(_.coord != candidate.coord).filter(_.coord != monitoring).exists(a => isBetween(monitoring, candidate.coord)(a.coord))
    }
    println("v", visible)
    if (visible.nonEmpty) {
      killed += visible(0)
      println(s"kill ${killed.size}", visible(0).coord)
    }
    i += 1
    j += 1
    if (i >= allAngles.size) i = 0
  }

/*  val killSequence = allAngles.map { angle =>
    val toKill = byangleAndDist.filter {
      asteroid => !killed.contains(asteroid) && asteroid.angle == angle
    }.minBy(_.dist)
    killed.add(toKill)
    toKill
  }*/

  //killSequence.zipWithIndex.foreach { case(asteroid, idx) => println(s"$idx $asteroid") }
      /*.groupBy { case (_, (angle, _)) => angle }
      .mapValues { v => v.sortWith { case ((_, (angle1, dist1)), (_, (angle2, dist2))) =>
        val result = if (angle1.compareTo(angle2) == 0) dist1.compareTo(dist2) else angle1.compareTo(angle2)
        result == -1
      }}*/
    /*.sortWith { case ((_, (angle1, dist1)), (_, (angle2, dist2))) =>
      val result = if (angle1.compareTo(angle2) == 0) dist1.compareTo(dist2) else angle1.compareTo(angle2)
      result == -1
    }*/

  println(allAngles)
  println("aaa", byangleAndDist)


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

