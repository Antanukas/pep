package example.aoc

import scala.collection.mutable
import scala.io.Source

object Day18 extends App {

  val input = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/input.in").getLines()


  val board = (for {
    line <- input
  } yield line.split("").map(_.charAt(0))).toArray


  case class Node(x: Int, y: Int, depth: Int, keys: Set[Char])

  val allMoves = Seq(1, 2, 3, 4).reverse

  def move(x: Int, y: Int, direction: Int): (Int, Int) = direction match {
    case 1 => (x, y - 1)
    case 2 => (x, y + 1)
    case 3 => (x - 1, y)
    case 4 => (x + 1, y)
  }

  val keys = ('a' to 'z').toSet
  val doors = ('A' to 'Z').toSet

  var startX, startY = 0

  var allKeysToFind = mutable.Set.empty[Char]

  {
    var i = 0

    while (i < board.length) {
      var j = 0
      while (j < board(i).length) {
        if (board(i)(j) == '@') {
          startX = i
          startY = j
        }
        if (keys.contains(board(i)(j))) {
          allKeysToFind += board(i)(j)
        }
        j += 1
      }
      i += 1
    }
  }

  val cache = mutable.Set.empty[AnyRef]
  var totalIterations = 0
  var limit = 1000000

  def buildTree(cx: Int, cy: Int, depth: Int, visitCount: Int, visited: Map[(Int, Int), Int], currentKeys: Set[Char]): Seq[Node] = {
    /*totalIterations += 1
    if (totalIterations > limit) return Seq.empty*/

   // if (depth > 100) return Seq.empty
    val newVisitCount = if (keys.contains(board(cx)(cy)) && !currentKeys.contains(board(cx)(cy))) visitCount + 1 else visitCount
    val newCurrentKey = if (keys.contains(board(cx)(cy))) currentKeys + board(cx)(cy) else currentKeys
    val newVisited = visited.updated((cx, cy), visited.getOrElse((cx, cy), 0) + 1)
    val n = Node(cx, cy, depth, newCurrentKey)

    val nextMoves = allMoves.map(d => move(cx, cy, d)).filter { case (x, y) =>
      board(x)(y) == '.' || keys.contains(board(x)(y)) || newCurrentKey.contains(board(x)(y).toLower)
    }

    println(s"Going [${(cx, cy)}]", depth, board(cx)(cy), newVisited.getOrElse((cx, cy), 0), newVisitCount, newCurrentKey, nextMoves.map(a => a -> newVisited.get(a)))
    Seq(n) ++ nextMoves
      .filter(coord => newVisited.getOrElse(coord, 0) <= newVisitCount)
      .filter(_ => currentKeys != allKeysToFind)
      .flatMap { case (x, y) =>
        buildTree(x, y, depth + 1, newVisitCount, newVisited, newCurrentKey)
      }
  }

  println(startX, startY)
  board(startX)(startY) = '.'
  val nodes = buildTree(startX, startY, 0, 0, Map((startX, startY) -> 0), Set.empty)

  println("Opa opa", allKeysToFind, nodes)

  //println(nodes.find(_.keys.nonEmpty))
  println(nodes.find(_.keys == allKeysToFind).minByOption(_.depth))

}
