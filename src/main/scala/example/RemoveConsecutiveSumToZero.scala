package example

import scala.collection.mutable
import scala.io.Source

object RemoveConsecutiveSumToZero extends App {

  def solve(numbers: String): String = {
    answer(solve(fromString(numbers)))
  }
  //10, 15, 12 9 10 14 10
  // 10 -> 5 -> -3 -> -3 -> 1 -> 4 -> -4
  def solve(numbers: Node): Node = {
    println("sums", sums(numbers))
    println("answ", answer(numbers))

    var begining = numbers
    var current = Option(numbers)

    var seen = mutable.Map[Long, Node]()
    while (current.isDefined) {
      if (current.get.value == 0) {
        // just proceed
      } else if (current.get.sum == 0) {
        begining = current.get.next.get
        seen = mutable.Map[Long, Node]()
      } else if (seen.contains(current.get.sum)) {
        val first = seen(current.get.sum)
        println("Cycle", first.value, current.get.value)
        first.next = current.get.next
        seen = mutable.Map[Long, Node]()
        seen.put(first.sum, first)
      } else {
        seen.put(current.get.sum, current.get)
      }
      //println("Seen", seen.keys)
      current = current.get.next
    }
    begining
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
    println("size", numbers.size)
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

  def sums(numbers: Node): String = {
    val s = new StringBuilder
    var current = Option(numbers)
    while (current.isDefined) {
      s.append(current.get.sum)
      if (current.get.next.isDefined) s.append(" ")
      current = current.get.next
    }
    s.toString()
  }

  class Node(val value: Int, var next: Option[Node], val sum: Long)

  println(solve("1 -6 4 -4 6"))
  assert(solve("1 -6 4 -4 6") == "1")
  println(solve("-1 1 0 4 0 1 -1"))
  assert(solve("-1 1 3 1 -1 4 1 -1") == "3 4")
  assert(solve("1 -1 3 4") == "3 4")
  assert(solve("10 5 -3 -3 1 5 -3 -3 1 4 -4") == "10")
  assert(solve("10 5 -3 -3 1 4 -4") == "10")
  assert(solve("1 2 3 4") == "1 2 3 4")
  assert(solve("3 -1 1 4") == "3 4")
  assert(solve("3 4 1 -1") == "3 4")

  //println(solve(Source.fromFile("/Users/antanasb/Code/wix/pep/list.txt").getLines().next()))
}
