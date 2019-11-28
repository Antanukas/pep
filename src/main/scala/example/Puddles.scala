package example

object Puddles extends App {

  def isDecreasing(d: Int) = d >= 0

  def isIncreasing(d: Int) = d < 0

  def capacity(arr: Array[Int]): Int = {
    var puddleSum = 0
    var i = 1
    var j = 0

    var terrain = 0
    var lastDirection = arr(0).compareTo(arr(1))
    while (i < arr.length - 1) {
      val nextDirection = arr(i).compareTo(arr(i + 1))
      println("e", arr(i))
      if (isIncreasing(lastDirection) && isDecreasing(nextDirection) || arr(j) <= arr(i)) {
        val length = i - j - 1 // +1 ?
        val lowest = Math.min(arr(i), arr(j))
        val maxWater = length * lowest
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
        terrain += arr(i)
      }

      lastDirection = nextDirection
      i += 1
    }
    println(puddleSum)
    puddleSum
  }

  assert(capacity(Array(0, 3, 0, 2, 0, 3, 0)) == 4)
  assert(capacity(Array(0, 1, 0, 1, 0, 1, 0)) == 2)
  assert(capacity(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) == 6)
  assert(capacity(Array(0, 3, 2, 1, 2, 3, 0)) == 4)
}
