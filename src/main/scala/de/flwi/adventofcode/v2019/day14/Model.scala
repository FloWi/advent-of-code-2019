package de.flwi.adventofcode.v2019.day14

import cats.kernel.Monoid

object Model {
  case class Element(name: String)
  case class Formula(inputs: Map[Element, Int], output: (Element, Int))
  object Formula {
    def fromString(str: String): Formula = {
      val List(ingredientsStr, outputStr) = str.split(" => ").toList

      val ingredients = ingredientsStr.split(", ").map(elementAmountFromString).toMap
      val output      = elementAmountFromString(outputStr)

      Formula(ingredients, output)
    }

    def elementAmountFromString(str: String): (Element, Int) = {
      val List(amount, elementName) = str.split(" ").toList
      (Element(elementName), amount.toInt)
    }
  }

  def parseInput(lines: List[String]): List[Formula] =
    lines.map(Formula.fromString)

  def createRecipeBook(formulas: List[Formula]): Map[(Element, Int), Map[Element, Int]] =
    formulas.map {
      case Formula(inputs, output) =>
        output -> inputs
    }.toMap

  def calculateOreAmountForOneFuel(recipeBook: Map[(Element, Int), Map[Element, Int]]): Int = {
    //start from fuel and walk down until you find recipes, that take only ORE

    @scala.annotation.tailrec
    def helper(demand: Map[Element, Int], elementsInStock: Map[Element, Int], recursionDepth: Int): Map[Element, Int] = {
      import cats.implicits._

      println(s"""demand: $demand
           |elementsInStock: $elementsInStock
           |recursionDepth: $recursionDepth
           |""".stripMargin)

      val (oreDemands, nonOreDemands) = demand
        .partition(_._1.name == "ORE")

      //check if some demands can be fulfilled from elements in stock
      val demandFromStock = nonOreDemands.keySet.intersect(elementsInStock.keySet)
      if (demandFromStock.nonEmpty) {
        println("can fulfill some of the demand from stock.")

        val (updatedDemand, updatedElementsInStock) = demandFromStock.foldLeft((demand, elementsInStock)) {
          case ((currentDemand, currentElementsInStock), element) =>
            val demand = currentDemand(element)
            val supply = currentElementsInStock(element)

            val (newDemand, newSupply) = if (demand > supply) {
              (demand - supply, 0)
            }
            else {
              (0, supply - demand)
            }

            (currentDemand.updated(element, newDemand), currentElementsInStock.updated(element, newSupply))
        }

        helper(updatedDemand.filter(_._2 > 0), updatedElementsInStock.filter(_._2 > 0), recursionDepth)
      }
      else {
        nonOreDemands.toList match {
          case Nil =>
            oreDemands

          case ::(demanded, rest) =>
            //find recipe for demanded element
            val recipeKey                        = recipeBook.keys.find(_._1 == demanded._1).get
            val recipe                           = recipeBook(recipeKey)
            val recipeAmount                     = recipeKey._2
            val demandAmount                     = demanded._2
            val numberOfRecipeApplicationsDouble = demandAmount.toDouble / recipeAmount
            val numberOfRecipeApplications       = math.ceil(numberOfRecipeApplicationsDouble).toInt

            val isOverproduction     = numberOfRecipeApplicationsDouble.toInt != numberOfRecipeApplications
            val totalProduced        = numberOfRecipeApplications * recipeAmount
            val overProductionAmount = totalProduced - demandAmount

            val newDemands = recipe.map { case (element, i) => element -> i * numberOfRecipeApplications }

            println(s"""depth: $recursionDepth
                     |  demanded: ${demanded._2}x${demanded._1.name}.
                     |    Recipe ingredients: ${recipe.map { case (element, i) => s"${i}x${element.name}" }}
                     |    Recipe produces ${recipeAmount}x${recipeKey._1.name}.
                     |    Recipe applications needed: $numberOfRecipeApplications
                     |    Overproduction: $isOverproduction --> ${overProductionAmount}
                     |    Total demanded ingredients:  ${newDemands.map { case (element, i) => s"${i}x${element.name}" }}
                     |
                     |    """.stripMargin)

            val newOverProduction  = Map(recipeKey._1 -> overProductionAmount)
            val newElementsInStock = Monoid.combine(newOverProduction, elementsInStock).filter(_._2 > 0)
            val newDemand          = Monoid.combineAll(List(newDemands, oreDemands, rest.toMap))
            helper(newDemand, newElementsInStock, recursionDepth + 1)
        }
      }
    }

    val finalResult = helper(demand = Map(Element("FUEL") -> 1), elementsInStock = Map.empty, recursionDepth = 0)
    println(s"finalResult: $finalResult")

    finalResult.head._2
  }
}
