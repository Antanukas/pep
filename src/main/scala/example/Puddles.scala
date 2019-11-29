package example

object Puddles extends App {

  def trap(height: Array[Int]): Int = {
    if (height.size < 2) return 0
    var i = 1
    val leftHighest = new Array[Int](height.length)
    leftHighest(0) = height(0)
    while (i < height.length) {
      leftHighest(i) = Math.max(leftHighest(i - 1), height(i))
      i += 1
    }

    val rightHighest = new Array[Int](height.length)
    rightHighest(height.length - 1) = height(height.length - 1)
    i = height.length - 2
    while (i >= 0) {
      rightHighest(i) = Math.max(rightHighest(i + 1), height(i))
      i -= 1
    }

    i = 1
    var res = 0
    while (i < height.length - 1) {
      val maxWater = Math.min(leftHighest(i), rightHighest(i))
      res = res + (maxWater - height(i))
      i += 1
    }
    res
  }

  assert(trap(Array(5, 4, 1, 2)) == 1)
  assert(trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) == 6)
  assert(trap(Array(3, 0, 2, 0, 3)) == 7)
  assert(trap(Array(1, 0, 1, 0, 1)) == 2)
  assert(trap(Array(5, 2, 1, 2, 1, 5)) == 14)
  assert(trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) == 6)
  assert(trap(Array(3, 2, 1, 2, 3)) == 4)
}
