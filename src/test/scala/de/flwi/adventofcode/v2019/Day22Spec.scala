package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.Day22.{Cut, DealIntoNewStack, DealWithIncrement}
import org.scalatest.{FunSpec, Matchers}

class Day22Spec extends FunSpec with Matchers {
  describe("Part 1") {
    it("should generate a new stack correctly") {
      Day22.generateStack(10).mkString(" ") shouldBe "0 1 2 3 4 5 6 7 8 9"
    }

    it("should 'deal into new stack correctly'") {
      val expectedOutput = "9 8 7 6 5 4 3 2 1 0"

      val stack: Vector[Int]  = Day22.generateStack(10)
      val actual: Vector[Int] = Day22.dealIntoNewStack(stack)

      actual.mkString(" ") shouldBe expectedOutput
    }

    it("should 'deal with increment correctly'") {
      val expectedOutput = "0 7 4 1 8 5 2 9 6 3"

      val stack: Vector[Int]  = Day22.generateStack(10)
      val actual: Vector[Int] = Day22.dealWithIncrement(stack, 3)

      actual.mkString(" ") shouldBe expectedOutput
    }

    it("should cut correctly from the top of the deck") {
      val actual = Day22.cut(Day22.generateStack(10), 3)

      actual.mkString(" ") shouldBe "3 4 5 6 7 8 9 0 1 2"
    }

    it("should cut correctly from the bottom of the deck") {
      val actual = Day22.cut(Day22.generateStack(10), -4)

      actual.mkString(" ") shouldBe "6 7 8 9 0 1 2 3 4 5"
    }

    it("should parse the input correctly") {
      val input = """deal with increment 64
                    |deal into new stack
                    |cut 1004
                    |""".stripMargin

      val actual = input
        .split("\n")
        .filterNot(_.isEmpty)
        .toList
        .map(Day22.parseInstruction)

      val expected = List(DealWithIncrement(64), DealIntoNewStack, Cut(1004))

      actual should contain theSameElementsInOrderAs expected
    }

    describe("example 1") {
      val input = """deal with increment 7
                    |deal into new stack
                    |deal into new stack""".stripMargin

      val expectedResult = "0 3 6 9 2 5 8 1 4 7"

      it("should solve") {
        val stack  = Day22.generateStack(10)
        val actual = Day22.applyInstructionSet(stack, input.split("\n").toList)

        actual.mkString(" ") shouldBe expectedResult
      }
    }
  }

  describe("Part 2") {
    describe("mini example") {
      it("should find origin index of a card before applying the DealIntoNewStack") {
        Day22.findOriginIndex(stackSize = 10L, instruction = DealIntoNewStack, indexOfInterest = 0) shouldBe 9
        Day22.findOriginIndex(stackSize = 10L, instruction = DealIntoNewStack, indexOfInterest = 3) shouldBe 6
        Day22.findOriginIndex(stackSize = 10L, instruction = DealIntoNewStack, indexOfInterest = 9) shouldBe 0
      }

      it("should find origin index of a card before applying the DealWithIncrement") {
        Day22.findOriginIndex(stackSize = 10L, instruction = DealWithIncrement(3), indexOfInterest = 0) shouldBe 0
        Day22.findOriginIndex(stackSize = 10L, instruction = DealWithIncrement(3), indexOfInterest = 3) shouldBe 1
        Day22.findOriginIndex(stackSize = 10L, instruction = DealWithIncrement(3), indexOfInterest = 9) shouldBe 3
      }
    }
  }
}
