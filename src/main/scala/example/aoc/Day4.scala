package example.aoc

object Day4 extends App {
  val (start, end) = (372304, 847060)
  //val (start, end) = (788899, 788899)

  var conditions = for {
    candidate <- start to end
  } yield {
    val str = candidate.toString.toCharArray
    var i = 1
    var isIncreasing = true
    var duplicateLength = 1
    var wasDoubleDigitDuplicate = false
    var lastDigit = str(0)
    while (i < str.length) {
      if (str(i) < lastDigit) isIncreasing = false
      if (str(i) == lastDigit) {
        duplicateLength += 1
      } else {
        if (duplicateLength == 2) wasDoubleDigitDuplicate = true
        duplicateLength = 1
      }
      lastDigit = str(i)
      i += 1
    }

    wasDoubleDigitDuplicate = wasDoubleDigitDuplicate || duplicateLength == 2
    val result = wasDoubleDigitDuplicate && isIncreasing
    result
  }

  println(conditions.count(identity))

}
