package de.flwi.adventofcode.v2019
import de.flwi.adventofcode.v2019.Location.AsteroidMap
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

class Day10Spec extends FunSpec with Matchers {

  /*
      The best location for a new monitoring station on this map is the highlighted asteroid at 3,4
      because it can detect 8 asteroids, more than any other location.
      (The only asteroid it cannot detect is the one at 1,0;
      its view of this asteroid is blocked by the asteroid at 2,2.)
      All other asteroids are worse locations; they can detect 7 or fewer other asteroids.
      Here is the number of other asteroids a monitoring station on each asteroid could detect:

      .7..7
      .....
      67775
      ....7
      ...87

      Here is an asteroid (#) and some examples of the ways its line of sight might be blocked.
      If there were another asteroid at the location of a capital letter,
      the locations marked with the corresponding lowercase letter would be blocked
      and could not be detected:

      #.........
      ...A......
      ...B..a...
      .EDCG....a
      ..F.c.b...
      .....c....
      ..efd.c.gb
      .......c..
      ....f...c.
      ...e..d..c

      map-size: 10x10
      start at 0,0
        - foreach asteroid x
          - A
            - location (3,1)
            - offset (3,1)
            - normalize offset (3,1)
            - calculate line of sight
            - (3,1), (6,2), (9,3)
          - B
            - location (3,2)
            - offset (3,2)
            - normalize (3,2)
            - calculate line of sight
            - (3,2), (6,4), (9,6)
          - C
            - location (3,3)
            - offset (3,3)
            - normalize (1,1)
            - calculate line of sight
            - (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (9,9)
   */

  describe("Day 10 Part 1") {

    it("should calculate the line of sight") {
      val expected = List(
        //Location(3, 1), neither start nor end are part of the line of sight
        Location(6, 2),
        Location(9, 3)
      )
      val actual = Location.lineOfSight(Location(0, 0), Location(3, 1), 10, 10)
      actual should contain theSameElementsAs expected
    }

    it("should calculate the line of sight only in the direction of the candidate") {
      val expected = List(
        Location(0, 0),
        Location(2, 2)
      )
      val actual = Location.lineOfSight(Location(3, 3), Location(1, 1), 10, 10)
      actual should contain theSameElementsAs expected
    }

    it("should calculate the line of sight of 1x5 map") {
      val actual = Location.lineOfSight(Location(0, 0), Location(0, 2), 1, 5)
      actual should contain theSameElementsAs List(
        Location(0, 1),
        Location(0, 3),
        Location(0, 4)
      )
    }

    it("should calculate the line of sight for (1,0) --> (4,4) correctly") {
      val actual = Location.lineOfSight(Location(1, 0), Location(4, 4), 10, 10)
      actual should contain theSameElementsAs List(Location(7, 8))
    }

    it("should calculate the line of sight for (4,2) --> (2,2) correctly") {
      val actual = Location.lineOfSight(Location(4, 2), Location(2, 2), 10, 10)
      actual should contain theSameElementsAs List(
        Location(3, 2),
        Location(1, 2),
        Location(0, 2),
      )
    }

    it("normalized should work in every direction") {
      val origin = Location(3, 3)

      //left
      (origin - Location(1, 3)).normalized shouldBe Location(-1, 0)

      //right
      (origin - Location(5, 3)).normalized shouldBe Location(1, 0)

      //top
      (origin - Location(3, 1)).normalized shouldBe Location(0, -1)

      //bottom
      (origin - Location(3, 5)).normalized shouldBe Location(0, 1)

      //top-right
      (origin - Location(4, 2)).normalized shouldBe Location(1, -1)

      //bottom-right
      (origin - Location(4, 4)).normalized shouldBe Location(1, 1)

      //top-left
      (origin - Location(2, 2)).normalized shouldBe Location(-1, -1)

      //bottom-left
      (origin - Location(2, 4)).normalized shouldBe Location(-1, 1)

    }

    it("should calculate the line of sight where initial offset is (3,3) --> can be normalized") {

      val expected = (1.until(3) ++ 4.to(9)).map(x => Location(x, x))

      val actual = Location.lineOfSight(Location(0, 0), Location(3, 3), 10, 10)
      actual should contain theSameElementsAs expected
    }

    it("should parse an asteroid map correctly") {
      val input       = """.#..#
                          |.....
                          |#####
                          |....#
                          |...##
                          |""".stripMargin
      val asteroidMap = Location.parseMap(input)
      asteroidMap.asteroidLocations should contain theSameElementsAs List(
        (1, 0),
        (4, 0),
        (0, 2),
        (1, 2),
        (2, 2),
        (3, 2),
        (4, 2),
        (4, 3),
        (3, 4),
        (4, 4)
      ).map(Location.fromTuple)

      asteroidMap.width shouldBe 5
      asteroidMap.height shouldBe 5
    }

    it("should find the the visible asteroids correctly") {
      val input =
        """.#..#
          |.....
          |#####
          |....#
          |...##
          |""".stripMargin

      val asteroidMap = Location.parseMap(input)
      Location.findVisible(asteroidMap, Location(1, 0)).size shouldBe 7
      Location.findVisible(asteroidMap, Location(4, 0)).size shouldBe 7
      Location.findVisible(asteroidMap, Location(0, 2)).size shouldBe 6
      Location.findVisible(asteroidMap, Location(1, 2)).size shouldBe 7
      Location.findVisible(asteroidMap, Location(2, 2)).size shouldBe 7
      Location.findVisible(asteroidMap, Location(3, 2)).size shouldBe 7
      Location.findVisible(asteroidMap, Location(4, 2)).size shouldBe 5
      Location.findVisible(asteroidMap, Location(4, 3)).size shouldBe 7
      Location.findVisible(asteroidMap, Location(3, 4)).size shouldBe 8
      Location.findVisible(asteroidMap, Location(4, 4)).size shouldBe 7
    }

    it("should not see one asteroid behind another one") {
      val input =
        """
#
.
#
#
#
""".trim

      val asteroidMap = Location.parseMap(input)
      Location.canSee(Location(0, 0), Location(0, 2), asteroidMap) shouldBe true
      Location.canSee(Location(0, 0), Location(0, 3), asteroidMap) shouldBe false
      Location.canSee(Location(0, 0), Location(0, 4), asteroidMap) shouldBe false

    }

    it("canSee should only see the first in direct line of sight") {
      val input =
        """.#..#
          |.....
          |#####
          |....#
          |...##
          |""".stripMargin

      val asteroidMap = Location.parseMap(input)

      Location.canSee(Location(4, 2), Location(3, 2), asteroidMap) shouldBe true
      Location.canSee(Location(4, 2), Location(2, 2), asteroidMap) shouldBe false
      Location.canSee(Location(4, 2), Location(1, 2), asteroidMap) shouldBe false
      Location.canSee(Location(4, 2), Location(0, 2), asteroidMap) shouldBe false
    }

    it("part 1 small example should work") {
      val input =
        """.#..#
          |.....
          |#####
          |....#
          |...##
          |""".stripMargin

      val expected = Location(3, 4)
      val actual   = Location.findBestAsteroidForMonitoringStation(input)._1

      actual shouldBe expected
    }

    it("should solve part1") {
      println(Day10.part1(Day10.getInput.unsafeRunSync()))
    }
  }

  describe("Day 10 Part 1 large examples") {
    it("should solve example #1") {
      val input =
        """
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
        """.trim

      val map       = Location.parseMap(input)
      val candidate = Location(1, 8)
      val visibles  = Location.findVisible(map, candidate)

      val result = Location.findBestAsteroidForMonitoringStation(input)

      debugMap(map, visibles, candidate)

      result._2.size shouldBe 33
      result._1 shouldBe Location(5, 8)
    }

    it("should solve example #2") {
      val input =
        """
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
        """.trim

      Location.findBestAsteroidForMonitoringStation(input)._1 shouldBe Location(1, 2)
    }

    it("should solve example #3") {
      val input =
        """
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
        """.trim

      Location.findBestAsteroidForMonitoringStation(input)._1 shouldBe Location(6, 3)
    }

    it("should solve example #4") {
      val input =
        """
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
        """.trim

      Location.findBestAsteroidForMonitoringStation(input)._1 shouldBe Location(11, 13)
    }
  }

  def debugMap(map: AsteroidMap, visibles: Set[Location], candidate: Location): Unit = {

    def emptyMap(): ArrayBuffer[ArrayBuffer[Char]] = collection.mutable.ArrayBuffer.fill(map.height)(collection.mutable.ArrayBuffer.fill(map.width)(' '))

    def render(showMap: ArrayBuffer[ArrayBuffer[Char]]): String =
      showMap.map { row =>
        row.mkString(";")
      }.mkString("\n")

    val asteroidDebugMap = emptyMap()
    map.asteroidLocations.foreach { case Location(x, y) => asteroidDebugMap(y).update(x, '#') }

    println("asteroids")
    println(render(asteroidDebugMap))

    val visiblesDebugMap = emptyMap()
    visibles.foreach { case Location(x, y) => visiblesDebugMap(y).update(x, 'v') }
    visiblesDebugMap(candidate.y).update(candidate.x, 'x')

    println(s"point of view of $candidate")
    println(render(visiblesDebugMap))

  }

}
