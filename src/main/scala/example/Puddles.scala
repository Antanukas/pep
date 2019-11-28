package example

object Puddles extends App {

  def trap(height: Array[Int]): Int = {
    var res = 0

    var i = 1
    while (i < height.length - 1) {

      var left = height(i)
      var j = 0
      while (j < i) {
        left = Math.max(left, height(j))
        j += 1
      }

      var right = height(i)
      j = i + 1
      while (j < height.length) {
        right = Math.max(right, height(j))
        j += 1
      }

      res = res  + (Math.min(left, right) - height(i))
      i +=1
    }

    println(res)
    res
  }

  assert(trap(Array(5, 2, 1, 2, 1, 5)) == 14)
  assert(trap(Array(5, 4, 1, 2)) == 1)
  assert(trap(Array(3, 0, 2, 0, 3)) == 7)
  assert(trap(Array(1, 0, 1, 0, 1)) == 2)
  assert(trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) == 6)
  assert(trap(Array(3, 2, 1, 2, 3)) == 4)
}
