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

  assert(solveDumb(Array(3, 4, -1, 1)) == 2)
  assert(solveDumb(Array(1, 2, 3, 4, 5)) == 6)
  assert(solveDumb(Array(1, 2, 4, 5)) == 3)
  assert(solveDumb((1 until (Int.MaxValue/ 4)).toArray) == Int.MaxValue)
}
