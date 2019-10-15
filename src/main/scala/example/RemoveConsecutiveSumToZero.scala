package example

import scala.collection.mutable
import scala.io.Source

object RemoveConsecutiveSumToZero extends App {

  def solve(numbers: Node): Node = {
    var beginning = numbers
    var current = Option(numbers)

    var sums = mutable.Map[Long, Node]()
    while (current.isDefined) {
      // Handle corner cases when begins with zero sequence or ends with zero sequence
      if (current.get.sum == 0) {
        beginning = current.get.next.get
        sums = mutable.Map[Long, Node]()
      } else if (sums.contains(current.get.sum)) {
        val first = sums(current.get.sum)
        var cleaner = first.next.get
        while (!current.contains(cleaner)) {
          sums.remove(cleaner.sum)
          cleaner = cleaner.next.get
        }
        first.next = current.get.next
      } else {
        sums.put(current.get.sum, current.get)
      }
      current = current.get.next
    }
    beginning
  }

  class Node(val value: Int, var next: Option[Node], val sum: Long)

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
    println("result", answer(solve(input)))
    println(s"Solved in: ${System.currentTimeMillis() - startTime}ms")
  }

  def fromString(s: String): Node = {
    val numbers = s.split(" ").map(_.toInt)
    val first = new Node(numbers(0), next = None, sum = numbers(0))
    var current = first
    var i = 1
    while (i < numbers.length) {
      if (numbers(i) != 0) {
        val node = new Node(numbers(i), next = None, sum = current.sum + numbers(i))
        current.next = Some(node)
        current = node
      }

      i += 1
    }
    first
  }
  
  def answer(numbers: Node): String = {
    val s = new StringBuilder
    var current = Option(numbers)
    while (current.isDefined) {
      s.append(current.get.value)
      if (current.get.next.isDefined) s.append(" ")
      current = current.get.next
    }
    s.toString()
  }

  def printSums(numbers: Node): String = {
    val s = new StringBuilder
    var current = Option(numbers)
    while (current.isDefined) {
      s.append(current.get.sum)
      if (current.get.next.isDefined) s.append(" ")
      current = current.get.next
    }
    s.toString()
  }

  def solve(numbers: String): String = {
    answer(solve(fromString(numbers)))
  }
}
