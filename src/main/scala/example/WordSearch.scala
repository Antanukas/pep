package example

import scala.io.Source

object WordSearch extends App {

  def solveSimple(grid: String, target: String): Boolean = solveSimple(grid.split("\n"), target)

  def solveSimple(grid: Seq[String], target: String): Boolean = solveSimple(grid.map(_.toCharArray.toVector).toVector, target.toCharArray.toVector)

  def solveSimple(grid: Vector[Vector[Char]], target: Vector[Char]): Boolean = {

    def findWord(g: Vector[Vector[Char]]): Boolean = {
      val allPossibleWords = g.map(row => row.combinations(target.length))
      allPossibleWords.collectFirst { case rowWords if rowWords.contains(target) => true }.isDefined
    }
    findWord(grid) || findWord(grid.transpose)
  }

  def prettyPrint(grid: Seq[Seq[Char]]): Unit = {
    println(grid.map(_.mkString("")).mkString("\n"))
  }

  assert(
    solveSimple(
      """FACI
        |OBQP
        |ANOB
        |MASS""".stripMargin,
      target = "FOAM") == true
  )

  val input = Source.fromFile("/Users/antanasb/Code/wix/pep/matrix2000x2000.txt").getLines().toSeq

  (1 to 5).foreach(_ => solveSimple(input, target = "B" * 1000))
  val start = System.currentTimeMillis()
  assert(
    solveSimple(
      input,
      target = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB") == false
  )
  println(s"Large done in ${System.currentTimeMillis() - start}ms")
}
