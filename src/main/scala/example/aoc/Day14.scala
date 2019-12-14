package example.aoc

import scala.collection.concurrent.TrieMap
import scala.io.Source

object Day14 extends App {

  case class Recipie(ingredients: Map[String, Long], result: String, amount: Long)

  def parseIngredient(ingredient: String): (String, Long) = {
    val Array(amount, key) = ingredient.trim.split(" ")
    key -> amount.toLong
  }

  val recipies = for {
    line <- Source.fromFile("/Users/antanasb/Code/wix/pep/src/main/scala/example/aoc/input.in").getLines().toSeq
    //7 A, 1 E => 1 FUEL
    s"$recipie => $result" = line

    ingredients = recipie.split(",")

    (resultingIngredient, resultingAmount) = parseIngredient(result)
  } yield Recipie(ingredients.map(parseIngredient).toMap, resultingIngredient, resultingAmount)

  val byResult = recipies.groupBy(_.result).view.mapValues(_.head).toMap

  val need = TrieMap.empty[String, Long]

  def countFor(ingredient: String, amount: Long): Unit = {
    val recipie = byResult(ingredient)

    val multiplier = Math.ceil((amount * 1.0) / recipie.amount).toLong
    need(ingredient) -= multiplier * recipie.amount

    recipie.ingredients.foreach { case (subIngredient, subAmount) =>
      if (subIngredient == "ORE") need("ORE") = need.getOrElseUpdate("ORE", 0) + subAmount * multiplier else {
        need(subIngredient) = need.getOrElseUpdate(subIngredient, 0) + subAmount * multiplier
        countFor(subIngredient, need(subIngredient))
      }
    }
  }

 /* need.put("FUEL", 1)
  val result = countFor("FUEL", 1)
  // P1
  println(need("ORE"))*/


  // P2

  var tryFuel = 4200000
  var f = 0L
  while (f <= 1000000000000L) {
    val fuel = tryFuel
    need.put("FUEL", fuel)
    val result = countFor("FUEL", tryFuel)
    // P1
    println(need("ORE"), tryFuel)
    f = need("ORE")
    need.clear()
    tryFuel += 1
  }


  //1067449734973
  //1000000000000

}
