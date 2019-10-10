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

    def findWord(currentRow: Int, currentTarget: Array[Char], found: Array[Char]): Boolean = {
      println(currentRow)
      if (currentRow >= grid.length && !found.sameElements(target)) false
      else if (found.sameElements(target)) true
      else if (grid(currentRow).containsSlice(currentTarget)) {
        val newFound = found ++ currentTarget
        val newCurrentTarget = target.slice(target.indexOfSlice(newFound) + newFound.length, target.length)
        findWord(currentRow + 1, newCurrentTarget, newFound)
      } else if (currentTarget.length > 0) {
        findWord(currentRow, currentTarget.slice(0, currentTarget.length - 1), found)
      } else {
        findWord(currentRow - 1, currentTarget.slice(0, currentTarget.length - 1), found)
      }
    }
    findWord(0, target, Array())
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
        target = "rarcdbxbaa") == true
    )
  }
}
