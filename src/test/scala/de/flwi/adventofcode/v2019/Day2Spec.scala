package de.flwi.adventofcode.v2019

import org.scalatest.FunSuite
import org.scalatest.Matchers
class Day2Spec extends FunSuite with Matchers {

  val input = Day2.getInput.unsafeRunSync()
  val ints = input.split(",").toList.map(_.toInt)

  test("Part1 initial") {
    Day2.intCodeProgram("1,9,10,3,2,3,11,0,99,30,40,50") shouldBe "3500,9,10,70,2,3,11,0,99,30,40,50"
  }
  test("Part1 ex1 (1 + 1 = 2)") {
    Day2.intCodeProgram("1,0,0,0,99") shouldBe "2,0,0,0,99"
  }
  test("Part1 ex2 (3 * 2 = 6)") {
    Day2.intCodeProgram("2,3,0,3,99") shouldBe "2,3,0,6,99"
  }
  test("Part1 ex3 (99 * 99 = 9801)") {
    Day2.intCodeProgram("2,4,4,5,99,0") shouldBe "2,4,4,5,99,9801"
  }
  test("Part1 ex4") {
    Day2.intCodeProgram("1,1,1,4,99,5,6,0,99") shouldBe "30,1,1,4,2,5,6,0,99"
  }

  test("Part1 solution") {
    val actual = Day2.part1(input)
    actual.split(",").head shouldBe "3790689"
  }

  test("Part2 solution") {
    val actual = Day2.part2(input)
    val (noun, verb) = actual
    val result = Day2.calculateResultWithUpdatedNounAndVerb(ints, noun, verb)

    println(actual)
    println(result)
    result.head shouldBe 19690720

  }
}
