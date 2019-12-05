package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.Day3.{Down, Point}
import org.scalatest.{FunSuite, Matchers}

class Day4Spec extends FunSuite with Matchers {

  import Day4._

  test("is6Digit") {
    is6DigitsLong(1000) shouldBe false
    is6DigitsLong(5555) shouldBe false
    is6DigitsLong(100000) shouldBe true
    is6DigitsLong(555555) shouldBe true
  }

  test("containsTwoAdjacentDigitsThatAreTheSame") {
    containsTwoAdjacentDigitsThatAreTheSame(112345) shouldBe true
    containsTwoAdjacentDigitsThatAreTheSame(123456) shouldBe false
  }

  test("onlyIncreasingIntegers") {
    onlyIncreasingIntegers(123456) shouldBe true
    onlyIncreasingIntegers(122334) shouldBe true
    onlyIncreasingIntegers(122330) shouldBe false
  }

  test("containsTwoAdjacentDigitsThatAreTheSameButNotPartOfLargerGroup") {
    containsTwoAdjacentDigitsThatAreTheSameButNotPartOfLargerGroup(123444) shouldBe false //44 is part of 444
    containsTwoAdjacentDigitsThatAreTheSameButNotPartOfLargerGroup(111122) shouldBe true //1 is repeated more than twice, but 22 counts
  }

}
