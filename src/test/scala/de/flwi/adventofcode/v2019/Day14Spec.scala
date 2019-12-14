package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.day14.Day14
import de.flwi.adventofcode.v2019.day14.Model._
import org.scalatest.{FunSpec, Matchers}

class Day14Spec extends FunSpec with Matchers {
  describe("Part 1") {
    describe("example 1") {
      val input =
        """
9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL
          """.trim
      val expectedOreFor1Fuel = 165

      it("should parse correctly") {
        val actual = parseInput(input.split("\n").toList)

        actual.head shouldBe Formula(Map(Element("ORE") -> 9), (Element("A"), 2))
        val recipeStr = actual.map {
          case Formula(inputs, output) =>
            val inputsStr = inputs.map(i => s"${i._2} ${i._1.name}").mkString(", ")
            val outputStr = s"${output._2} ${output._1.name}"
            s"$inputsStr => $outputStr"
        }.mkString("\n")

        recipeStr shouldBe input
      }

      it("create a recipeBook correctly") {
        val recipeBook = createRecipeBook(parseInput(input.split("\n").toList))
        calculateOreAmountForOneFuel(recipeBook) shouldBe expectedOreFor1Fuel
      }
    }
    describe("example 2") {
      val input =
        """
157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
      """.trim

      val expectedOreFor1Fuel = 13312

      it("create a recipeBook correctly") {
        val recipeBook = createRecipeBook(parseInput(input.split("\n").toList))
        calculateOreAmountForOneFuel(recipeBook) shouldBe expectedOreFor1Fuel
      }
    }

    it("should solve part 1 correctly") {
      println(Day14.part1(Day14.getInput.unsafeRunSync()))
    }
  }

  describe("Part 2") {
    val amountORE = 1000000000000L

    describe("example 1") {
      val input =
        """
          |157 ORE => 5 NZVS
          |165 ORE => 6 DCFZ
          |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
          |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
          |179 ORE => 7 PSHF
          |177 ORE => 5 HKGWZ
          |7 DCFZ, 7 PSHF => 2 XJWVT
          |165 ORE => 2 GPVTF
          |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
          |""".stripMargin.trim

      it("should solve ex1") {
        val expected = 82892753L

        val formulas                                             = parseInput(input.split("\n").toList)
        val recipeBook: Map[(Element, Long), Map[Element, Long]] = createRecipeBook(formulas)

        val foo = calculateOreAmountForOneFuel(recipeBook, Map(Element("FUEL") -> expected))

        val result = searchMaxFuel(recipeBook, 1, amountORE, amountORE)
        println(result)
        result shouldBe expected
      }
    }

    it("should solve part 2 correctly") {
      val formulas = parseInput(Day14.getInput.unsafeRunSync())

      val recipeBook: Map[(Element, Long), Map[Element, Long]] = createRecipeBook(formulas)

      // search for the optimal solution
      // e.g. 100 fuel require 1000 ORE
      //      200 fuel require 2000 ORE

      val result = searchMaxFuel(recipeBook, 1, amountORE, amountORE)
      println(result)
    }
  }
}
