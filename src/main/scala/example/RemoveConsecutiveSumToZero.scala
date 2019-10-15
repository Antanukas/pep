package example

import scala.collection.mutable
import scala.io.Source

object RemoveConsecutiveSumToZero extends App {

  def solve(initialNumbers: Node): Node = {
    var beginning = initialNumbers
    var maybeCurrent = Option(initialNumbers)

    val sums = mutable.Map[Long, Node]()
    var sum = 0
    while (maybeCurrent.isDefined) {
      val current = maybeCurrent.get
      sum += current.value
      if (sum == 0) {
        beginning = current.next.get
        sums.clear()
      } else if (sums.contains(sum)) {
        sums(sum).next = current.next
      } else {
        sums.put(sum, current)
      }
      maybeCurrent = current.next
    }

    beginning
  }

  class Node(val value: Int, var next: Option[Node])

  assert(solve("1 -7 -6 4 -4 6 7") == "1")
  assert(solve("1 -6 4 -4 6") == "1")
  assert(solve("10 5 -3 -3 1 5 -3 -3 1 4 -4") == "10")
  assert(solve("-1 -2 4 -4 5 2 1") == "-1 -2 5 2 1")
  assert(solve("-1 1 3 1 -1 4 1 -1") == "3 4")
  assert(solve("1 -1 3 4") == "3 4")
  assert(solve("10 5 -3 -3 1 4 -4") == "10")
  assert(solve("1 2 3 4") == "1 2 3 4")
  assert(solve("3 -1 1 4") == "3 4")
  assert(solve("3 4 1 -1") == "3 4")

  {
    val input = fromString(Source.fromFile("/Users/antanasb/Code/wix/pep/list.txt").getLines().next())
    val startTime = System.currentTimeMillis()
    println("result", toString(solve(input)))
    println(s"Solved in: ${System.currentTimeMillis() - startTime}ms")
  }

  def fromString(s: String): Node = {
    val numbers = s.split(" ").map(_.toInt)
    val first = new Node(numbers(0), next = None)
    var current = first
    var i = 1
    while (i < numbers.length) {
      if (numbers(i) != 0) {
        val node = new Node(numbers(i), next = None)
        current.next = Some(node)
        current = node
      }

      i += 1
    }
    first
  }

  def toString(numbers: Node): String = {
    val s = new StringBuilder
    var current = Option(numbers)
    while (current.isDefined) {
      s.append(current.get.value)
      if (current.get.next.isDefined) s.append(" ")
      current = current.get.next
    }
    s.toString()
  }

  def solve(numbers: String): String = {
    toString(solve(fromString(numbers)))
  }
}
