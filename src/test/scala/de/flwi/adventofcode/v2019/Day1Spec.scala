package de.flwi.adventofcode.v2019

import org.scalatest.FunSuite
import org.scalatest.Matchers

class Day1Spec extends FunSuite with Matchers {

  test("Part1") {
    Day1.getFuel(12) shouldBe 2
    Day1.getFuel(14) shouldBe 2
    Day1.getFuel(1969) shouldBe 654
    Day1.getFuel(100756) shouldBe 33583
  }
  test("Part2") {
    Day1.getFuelRecursively(12) shouldBe 2
    Day1.getFuelRecursively(1969) shouldBe 966
    Day1.getFuelRecursively(100756) shouldBe 50346
  }

}
