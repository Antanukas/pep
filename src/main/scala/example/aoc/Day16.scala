package example.aoc

import scala.collection.mutable
import scala.io.Source

object Day16 extends App {

  val input = Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/input.in").getLines().next.split("").map(_.toLong)

  val phase = 101

  val patterns = {
    var i = 1
    //0, 1, 0, -1
    val p = mutable.Map.empty[Int, Seq[Int]]
    while (i < input.length + 1) {
      p += i - 1 -> (Seq.fill(i)(0) ++ Seq.fill(i)(1) ++ Seq.fill(i)(0) ++ Seq.fill(i)(-1))
      i += 1
    }
    p.toMap
  }



  {
    var i = 1
    var current = input
    while (i < phase) {

      var j = 0
      val output = current.clone()

      while (j < current.length) {
        //Math.abs((input * p) % 10)
        val pattern = LazyList.continually(patterns(j)).flatten
        val toSum = current.zip(pattern.drop(1)) // ??
          .map { case (input, p) => input * p }
        output(j) = Math.abs(toSum.sum % 10)
        j += 1
      }
      println(s"Phase $i", output.mkString(""))
      current = output
      i += 1
    }
  }
//73745418
//74608727092984117225488554779405012467436674564530073186253167855468009757406537939221716262764368124618687995931374115095325199580367437035793220898926419915005566828106364680417980752456693860303897506049113167085482332669866829805204830914751373128588737340088207520689634322276348986337314549631149122132256732352522087321233719828004869381958744548803900795953925855541413762750741129784619961087510635770430535329025019197154087230962893268649040873816286289787688438417745109573274967302546629701216129800804867837892404522245753913373117642249766266335489785134101349009071832229956086501209240163910201161195041969130533091665702047971622614


}
