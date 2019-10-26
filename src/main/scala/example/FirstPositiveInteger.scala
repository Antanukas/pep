package example

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

  assert(solve2(Array(3, 4, -1, 1)) == 2)
  assert(solve2(Array(1, 2, 3, 4, 5)) == 6)
  assert(solve2(Array(1, 2, 4, 5)) == 3)
  assert(solve2(Array(-1, 1, -1, 2, 4, 5, -1)) == 3)
  assert(solve2((1 until (Int.MaxValue / 8)).toArray) == (Int.MaxValue / 8))
}
