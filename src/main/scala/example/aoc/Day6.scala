package example.aoc

import scala.collection.mutable
import scala.io.Source

object Day6 extends App {
  var orbits = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/input.in").getLines()

  case class Node(name: String, parent: Option[Node], var children: Seq[Node]) {
    def print(): String = {
      s"""$name -> ${parent.map(_.name)}, ${children.map(_.print()).mkString(",")}"""
    }

    lazy val currentPath: Seq[Node] = parent.map(_.currentPath).getOrElse(Seq.empty) :+ this

    def findPath(toFind: String): Option[Seq[String]] = {
      if (name == toFind) Some(currentPath.map(_.name))
      else {
        var i = 0
        while (i < children.size) {
          val maybePath = children(i).findPath(toFind)
          if (maybePath.isDefined) return maybePath
          i += 1
        }
        None
      }
    }

    def orbitting: Int = currentPath.size + children.map(_.orbitting).sum
  }

  // build flat tree from which tree will be built
  var flatTree = mutable.Map.empty[String, Seq[String]]
  while (orbits.hasNext) {
    val Array(nodeName, orbiter) = orbits.next().split("\\)")
    val children = flatTree.getOrElseUpdate(nodeName, Seq.empty)
    flatTree.put(nodeName, children :+ orbiter)
  }

  def buildTree(nodeName: String, parent: Option[Node]): Node = {
    val node = Node(nodeName, parent = parent, children = Seq.empty)
    val children = flatTree.getOrElse(nodeName, Seq.empty).map(c => buildTree(nodeName = c, parent = Some(node)))
    node.children = children
    node
  }

  val nodes = buildTree("COM", parent = None)

  val you = nodes.findPath("YOU").get.toSet
  val san = nodes.findPath("SAN").get.toSet
  val commonPath = you.intersect(san)

  println(nodes.orbitting)
  println(you.union(san).diff(commonPath).size - 2)

}
