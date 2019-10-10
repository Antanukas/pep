package example

import example.algos.KMP

import scala.io.Source

object WordSearch extends App {

  def solveSimple(grid: String, target: String): Boolean = solveSimple(grid.split("\n"), target)

  def solveSimple(grid: Seq[String], target: String): Boolean = solveSimple(grid.map(_.toCharArray.toVector).toVector, target.toCharArray.toVector)

  def solveSimple(grid: Vector[Vector[Char]], target: Vector[Char]): Boolean = {

    def findWord(g: Vector[Vector[Char]]): Boolean = {
      val allPossibleWords = g.map(row => row.sliding(target.length, 1))
      allPossibleWords.collectFirst { case rowWords if rowWords.contains(target) => true }.isDefined
    }

    findWord(grid) || findWord(grid.transpose)
  }

  def solveBig(grid: String, target: String): Boolean = solveBig(grid.split("\n"), target)

  def solveBig(grid: Seq[String], target: String): Boolean = solveBig(grid.map(_.toCharArray).toArray, target.toCharArray)

  def solveBig(grid: Array[Array[Char]], target: Array[Char]): Boolean = {
    def findWord(g: Array[Array[Char]]): Boolean = g.exists(row => row.containsSlice(target))
    findWord(grid) || findWord(grid.transpose)
  }

  def solveKMP(grid: String, target: String): Boolean = solveKMP(grid.split("\n"), target)

  def solveKMP(grid: Seq[String], target: String): Boolean = solveKMP(grid.map(_.toCharArray).toArray, target.toCharArray)

  def solveKMP(grid: Array[Array[Char]], target: Array[Char]): Boolean = {
    val kmp = new KMP(target)
    def findWord(g: Array[Array[Char]]): Boolean = g.exists(row => kmp.search(row) != row.length)
    findWord(grid) || findWord(grid.transpose)
  }

  def solveCurved(grid: String, target: String): Boolean = solveCurved(grid.split("\n"), target)

  def solveCurved(grid: Seq[String], target: String): Boolean = solveCurved(grid.map(_.toCharArray).toArray, target.toCharArray)

  def solveCurved(grid: Array[Array[Char]], target: Array[Char]): Boolean = {

    var startedRowAt, startedAtColumn, currentRow, currentColumn, found = 0

    /*
       XFOXX
       XFOXX
       XXAMX
       XXXXX
     */
    while(currentRow < grid.length - 1) {
      if (startedAtColumn >= grid(currentRow).length - 1) {
        currentRow += 1
        startedRowAt = currentRow
        found = 0
        currentColumn = 0
        startedAtColumn = 0
      } else if (currentColumn >= grid(currentRow).length) {
        currentRow = startedRowAt
        found = 0
        currentColumn = startedAtColumn + 1
        startedAtColumn = currentColumn
      }

      var foundAtLeastOne = false
      while(grid(currentRow)(currentColumn) == target(found)
        && currentColumn < grid(currentRow).length - 1
      ) {
        if (found == target.length - 1) return true
        if (grid(currentRow)(currentColumn + 1) == target(found + 1)) {
          currentColumn += 1
        }
        found += 1
        foundAtLeastOne = true
      }
      if (currentColumn >= grid(currentRow).length || (found > 0 && !foundAtLeastOne)) {
        startedAtColumn += 1
        currentColumn = startedAtColumn
        found = 0
      }
      else if (foundAtLeastOne) currentRow += 1
      else currentColumn += 1
    }

    false
  }

  def prettyPrint(grid: Seq[Seq[Char]]): Unit = {
    println(grid.map(_.mkString("")).mkString("\n"))
  }

/*  assert(
    solveBig(
      """FACI
        |OBQP
        |ANOB
        |MASS""".stripMargin,
      target = "FOAM") == true
  )

  assert(
    solveKMP(
      """FACI
        |OBQP
        |ANOB
        |MASS""".stripMargin,
      target = "FOAM") == true
  )*/

  assert(
    solveCurved(
      """XFOXX
        |XFOXX
        |XXAMX
        |XXXXX""".stripMargin,
      target = "FOAM") == true
  )

  assert(
    solveCurved(
      """XFXXX
        |XOAXX
        |XXMXX
        |XXXXX""".stripMargin,
      target = "FOAM") == true
  )

  assert(
    solveCurved(
      """XFXXX
        |XOAXX
        |XXMXX
        |XXXXX""".stripMargin,
      target = "FOAMZZ") == false
  )

  val input = Source.fromFile("/Users/antanasb/Code/wix/pep/matrix2000x2000.txt").getLines().toSeq

  solveBig(input, target = "B" * 1000)

  {
    val start = System.currentTimeMillis()
    assert(
      solveBig(
        input,
        target = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB") == false
    )
    println(s"Large done in ${System.currentTimeMillis() - start}ms")
  }

  {
    val start = System.currentTimeMillis()
    assert(
      solveKMP(
        input,
        target = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB") == false
    )
    println(s"Large KMP done in ${System.currentTimeMillis() - start}ms")
  }

  {
    val abra = Source.fromFile("/Users/antanasb/Code/wix/pep/abracadabra-matrix.txt").getLines().toSeq
    assert(
      solveCurved(
        abra,
        target = "xaxbra") == true
    )
  }
}
