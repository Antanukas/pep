package example

object Puddles extends App {

  def isDecreasing(d: Int) = d >= 0

  def isIncreasing(d: Int) = d < 0

  def trap(height: Array[Int]): Int = {
    var puddleSum = 0
    var i = 1
    var j = 0

    val arr = height :+ (0)
    var lastDirection = 0
    while (i < arr.length - 1) {
      val nextDirection = arr(i).compareTo(arr(i + 1))
      println("e", arr(i))
      if (isIncreasing(lastDirection) && isDecreasing(nextDirection) || arr(j) <= arr(i)) {
        val length = i - j // +1 ?
        val lowest = Math.min(arr(i), arr(j))
        val maxWater = length * lowest

        var terrain = 0
        var k = j
        while (k < i) {
          terrain += Math.min(lowest, arr(k))
          k += 1
        }
        println("terrain", terrain)
        println("add to sum", i, j, maxWater, terrain, maxWater - terrain)
        puddleSum += maxWater - terrain
        terrain = 0

        if (isIncreasing(lastDirection) && isIncreasing(nextDirection)) {
          while (isIncreasing(arr(i).compareTo(arr(i + 1)))) {
            i += 1
          }
          lastDirection = 1
        }
        j = i
      } else {
        //terrain += arr(i)
      }

      lastDirection = nextDirection
      i += 1
    }
    println(puddleSum)
    puddleSum
  }

  assert(trap(Array(5, 2, 1, 2, 1, 5)) == 14)
  assert(trap(Array(5, 4, 1, 2)) == 1)
  assert(trap(Array(3, 0, 2, 0, 3)) == 4)
  assert(trap(Array(1, 0, 1, 0, 1)) == 2)
  assert(trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) == 6)
  assert(trap(Array(3, 2, 1, 2, 3)) == 4)
}
