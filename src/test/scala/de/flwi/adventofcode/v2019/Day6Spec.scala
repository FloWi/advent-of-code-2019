package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.Day6._
import org.scalatest.{FunSuite, Matchers}

class Day6Spec extends FunSuite with Matchers {

  private val input = getInput.unsafeRunSync()

  private val exampleInput = """
      |COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L
      |""".stripMargin.trim.split("\n").toList

  test("example") {

    val planetMap = parsePlanetMap(exampleInput)
    planetMap.size shouldBe 12

    planetMap("B") shouldBe Planet("B", Some("COM"))

    getNumberOfOrbitsOfPlanet(planetMap, planetMap("D")) shouldBe 3
    getNumberOfOrbitsOfPlanet(planetMap, planetMap("L")) shouldBe 7
    getNumberOfOrbitsOfPlanet(planetMap, planetMap("COM")) shouldBe 0

    getTotalNumberOfOribts(planetMap) shouldBe 42

    getOrbitChain(planetMap, "D").mkString("-") shouldBe "COM-B-C-D"
  }



  test("COM in the middle of the input") {

    val pm = parsePlanetMap(
      List(
        "A)B",
        "C)D",
        "COM)A",
        "B)C"
      )
    )

    getOrbitChain(pm, "D").mkString("-") shouldBe "COM-A-B-C-D"
  }
//
//  test("Part1 solution") {
//    println(Day6.part1(input))
//  }



  test("part2 example - findCommonCenter") {
    val pm = parsePlanetMap("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN".split("\n").toList)

    findCommonCenter(pm, "YOU", "SAN") shouldBe Some("D")
  }

  test("part2 example ") {
    val pm = parsePlanetMap("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN".split("\n").toList)

    val planetHopChain = getPlanetHopChain(pm, "YOU", "SAN")

    println(s"it takes ${planetHopChain.size} orbital transfers")
    println(planetHopChain.mkString("\n"))

    planetHopChain.size shouldBe 4
  }

  test("Part2 solution") {
    println(Day6.part2(input))
  }

}
