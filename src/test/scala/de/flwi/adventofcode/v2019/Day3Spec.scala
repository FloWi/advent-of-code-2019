package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.Day3.{Down, Point}
import org.scalatest.{FunSuite, Matchers}

class Day3Spec extends FunSuite with Matchers {

  //val input = Day3.getInput.unsafeRunSync()

  test("Down move") {
    Day3.getEndpoint(Point(0, 0), Down(30)) shouldBe Point(0, -30)
  }

  private val examplePath = List(
    Point(0, 0),
    Point(75, 0),
    Point(75, -30),
    Point(158, -30),
    Point(158, 53),
    Point(146, 53),
    Point(146, 4),
    Point(217, 4),
    Point(217, 11),
    Point(145, 11)
  )
  test("Part1 calculate moves and pathPoints from Directions") {

    val directions     = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    val expectedPoints = examplePath

    val moves = Day3.getMoves(directions)

    val actual: List[Point] = Day3.getPathPoints(Point(0, 0), moves)
    actual should contain theSameElementsInOrderAs (expectedPoints)
  }

  test("getPathPoints with one segment right") {
    Day3.getAllPointsOfPath(List(Point(1, 1), Point(3, 1))) should contain theSameElementsInOrderAs List(
      Point(1, 1),
      Point(2, 1),
      Point(3, 1)
    )
  }

  test("getPathPoints with one segment left") {
    Day3.getAllPointsOfPath(List(Point(3, 1), Point(1, 1))) should contain theSameElementsInOrderAs List(
      Point(3, 1),
      Point(2, 1),
      Point(1, 1)
    )
  }

  test("getPathPoints with two segments") {
    Day3.getAllPointsOfPath(List(Point(1, 1), Point(3, 1), Point(3, 3))) should contain theSameElementsInOrderAs List(
      Point(1, 1),
      Point(2, 1),
      Point(3, 1),
      Point(3, 2),
      Point(3, 3)
    )
  }

  test("example of description") {
    val result: Day3.IntersectionResult =
      Day3.intersections("R8,U5,L5,D3", "U7,R6,D4,L4")
    result.intersectionPoints should contain theSameElementsAs List(Point(3, 3), Point(6, 5))
  }

  test("full path should contain pathPoints of input path") {
    val path = examplePath

    val actual = Day3.getAllPointsOfPath(path)
    actual should contain allElementsOf path
  }

  test("manhattan distance of every path segment should be 1") {
    val path = examplePath

    val actual = Day3
      .getAllPointsOfPath(path)
      .sliding(2, 1)
      .toList
      .flatMap {
        case List(p1, p2) => List(Day3.manhattanDistance(p1, p2))
        case List(_) => List.empty //end of path
      }

    actual.distinct should contain theSameElementsAs List(1)
  }

  test("Part1 ex1") {
    Day3.part1(List("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")) shouldBe 159
  }

  test("Part2 ex1") {
    Day3.part2(List("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")) shouldBe 610
  }

  test("Part2 ex2") {
    Day3.part2(List("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) shouldBe 410
  }
//
//  test("Part1 ex1") {
//    Day3.solve("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") shouldBe 135
//  }
}
