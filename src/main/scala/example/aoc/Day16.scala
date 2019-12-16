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

  //1,     0,     -1,    0,     1,     0,    -1,     0,      1,     0,     -1,    0,     1,     0,     -1     0
  //1*1  + 2*0  + 3*-1 + 4*0  + 5*1  + 6*0  + 7*-1 + 8*0 + 1*1  + 2*0  + 3*-1 + 4*0  + 5*1  + 6*0  + 7*-1 + 8*0
  //                        23845678
  //02476902023901786284567862845678
  //               862845678
  //                3998217862845678

  //02840628
  val offset = input.take(7).mkString("").toInt
  //println(offset) // 5973847
  val normalizedOffset = (offset % input.length)
  println("normalized", offset % 10000) //3847

  /*
  (normalized,3510)
         02935109699940806955835062552853
(Phase 1,23310337723769636955835062552853)
(Result:,37723769)
         02935109699940807127241660838683
(Phase 2,20301664107212077127241660838683)
(Result:,64107212)
(Phase 3,72236468928663262542539826685713)
(Result:,68928663)
(Phase 4,40401061439373396495385686046143)
(Result:,61439373)
(Phase 5,15135332981565248289413824884873)
(Result:,32981565)
(Phase 6,65744741014728587979065242802803)
(Result:,41014728)
(Phase 7,50974882176320472569004973133133)
(Result:,82176320)
         02935109699940809726777347430763
(Phase 8,79216555854257339726777347430763)
   */
  {
    var i = 1
    val current = Seq.fill(10000)(input).flatten.toArray
    while (i < phase) {

      //var j = current.length / 2
      var j = offset - 1
      //val output = current.clone()

      var sum = 0L
      var k = j
      while (k < current.length) {
        sum += current(k)
        k += 1
      }

      var last = current(j)
      current(j) = sum % 10
      j += 1

      while (j < current.length) {
        sum -= last
        last = current(j)
        current(j) = sum % 10
        j += 1
        //Math.abs((input * p) % 10)
/*        val pattern = LazyList.continually(patterns(j)).flatten
        val toSum = current.zip(pattern.drop(1)) // ??
          .map { case (input, p) => input * p }
      //  println(toSum.mkString(","), toSum.sum)
        output(j) = Math.abs((toSum.sum) % 10)
        j += 1*/
      }
      /*
      (Phase 1,48226158)
(Phase 2,34040438)
(Phase 3,03415518)
(Phase 4,01029498)
(Phase 5,02140178)
(Phase 6,85526658)
(Phase 7,41395938)
(Phase 8,38765018)
(Phase 9,04824998)
(Phase 10,36440678)
(Phase 11,83871158)
(Phase 12,45645438)
       */
      //println(s"Phase $i", current.mkString(""))
      println(s"Phase $i")
      if (i == 100) {
        println(s"Result $i:", current.slice(offset, offset + 8).mkString(""))
      }
      i += 1
    }
  }
//73745418
//74608727092984117225488554779405012467436674564530073186253167855468009757406537939221716262764368124618687995931374115095325199580367437035793220898926419915005566828106364680417980752456693860303897506049113167085482332669866829805204830914751373128588737340088207520689634322276348986337314549631149122132256732352522087321233719828004869381958744548803900795953925855541413762750741129784619961087510635770430535329025019197154087230962893268649040873816286289787688438417745109573274967302546629701216129800804867837892404522245753913373117642249766266335489785134101349009071832229956086501209240163910201161195041969130533091665702047971622614


}
