package example

import scala.collection.mutable

object FirstPositiveInteger extends App {
  def solveDumb(arr: Array[Int]): Int = {
    val positives = arr.toSet.filter(_ > 0)
    val min = positives.min

    if (min > 1) min - 1
    else {
      val max = positives.max
      var i = min
      while (i <= max + 1) {
        if (!positives.contains(i)) {
          return i
        }
        i += 1
      }
      -1
    }
  }

  def solve2(arr: Array[Int]): Int = {
    var i = 0
    val sorted = arr.sorted
    var max = 0
    while (i < sorted.length) {
      if (sorted(i) < 1) {
        max = sorted(i + 1)
      } else if (sorted(i) == max + 1) {
        max += 1
      } else if (sorted(i) > max + 1) {
        return max + 1
      }
      i += 1
    }
    max + 1
  }

  def solve3(arr: mutable.HashSet[Int]): Int = {
    var i = 1
    var max = 1
    arr.foreach { e =>
      if (e > max) max = e
      if (!arr.contains(i)) return i
      if (e > 0) i += 1
    }
    max + 1
  }

/*
  6 1
  2 1
  1 3
  5 3
  4 3
  3
  */
  assert(solve3(mutable.HashSet(1, 2, 3, 2, 5, 1)) == 4)
  assert(solve3(mutable.HashSet(3, 4, -1, 1, 2, 6)) == 5)
  assert(solve3(mutable.HashSet(3, 4, -1, 1)) == 2)
  assert(solve3(mutable.HashSet(1, 2, 3, 4, 5)) == 6)
  assert(solve3(mutable.HashSet(1, 2, 4, 5)) == 3)
  assert(solve3(mutable.HashSet(-1, 1, -1, 2, 4, 5, -1)) == 3)

  val size = 10000000
  val long = {
    var i = 0
    val set = mutable.HashSet.newBuilder[Int]
    while (i < size) {
      set.addOne(i)
      i += 1
    }
    set.result()
  }
  assert(solve3(long) == size)
}
