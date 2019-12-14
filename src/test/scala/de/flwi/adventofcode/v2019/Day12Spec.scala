package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.day12.{Coord3D, Day12, Velocity}
import de.flwi.adventofcode.v2019.day12.Day12.{parseCoord, simulate}
import fs2.Pure
import org.scalatest.{FunSpec, Matchers}

class Day12Spec extends FunSpec with Matchers {

  describe("Part 1") {

    describe("fundamentals") {
      it("applyGravityCalcDeltaV should work") {
        val actual = Day12.applyGravityCalcDeltaV(
          List(
            (1, Coord3D(3, 0, 0)),
            (2, Coord3D(5, 0, 0))
          )
        )

        val expected = List(
          (1, Velocity(1, 0, 0)),
          (2, Velocity(-1, 0, 0))
        )

        actual shouldBe expected
      }
    }

    describe("example 1") {
      val input =
        """
          |<x=-1, y=0, z=2>
          |<x=2, y=-10, z=-7>
          |<x=4, y=-8, z=8>
          |<x=3, y=5, z=-1>
          |""".stripMargin.trim
          .split("\n")
          .toList
          .filterNot(_.isEmpty)

      it("should work for iteration 1") {
        val moons = input.map(parseCoord)

        val actual = simulate(moons)
          .take(11)
          .toList
          .zipWithIndex
          .map {
            case ((indexedMoons, indexedVelocities), idx) =>
              val total = indexedMoons.keys.toList.sorted.map { moonKey =>
                //val moonString = indexedMoons.keys.toList.sorted.map { moonKey =>
                val m = indexedMoons(moonKey)
                val v = indexedVelocities(moonKey)

                val pot   = m.total
                val kin   = v.total
                val total = pot * kin

                //     s"pos=<x=${m.x}, y=${m.y}, z=${m.z}> vel=<x=${v.x}, y=${v.y}, z=${v.z}>"
                total
              }.sum

              s"""After $idx steps:
             |$total
             |""".stripMargin
          }
          .mkString("\n")

        println(actual)

      }
    }

    it("should solve part 1") {
      val moons = Day12.getInput.unsafeRunSync().map(parseCoord)

      val actual = simulate(moons)
        .take(1001)
        .toList
        .zipWithIndex
        .map {
          case ((indexedMoons, indexedVelocities), idx) =>
            val total = indexedMoons.keys.toList.sorted.map { moonKey =>
              //val moonString = indexedMoons.keys.toList.sorted.map { moonKey =>
              val m = indexedMoons(moonKey)
              val v = indexedVelocities(moonKey)

              val pot   = m.total
              val kin   = v.total
              val total = pot * kin

              //     s"pos=<x=${m.x}, y=${m.y}, z=${m.z}> vel=<x=${v.x}, y=${v.y}, z=${v.z}>"
              total
            }.sum

            s"""After $idx steps:
                 |$total
                 |""".stripMargin
        }
        .mkString("\n")

      println(actual)

    }
  }

  describe("Part 2") {
    it("should solve part 2") {
      val moons = Day12.getInput.unsafeRunSync().map(parseCoord)

      val actual: fs2.Stream[Pure, (Map[Int, Coord3D], Map[Int, Velocity])] = simulate(moons)

      val initialPosition = actual.head.toList.head._1

      //check the recurrence for every moon to its origin independently?

      val set = collection.mutable.HashSet.empty[(Map[Int, Coord3D], Map[Int, Velocity])]

      actual.zipWithIndex.filter {
        case ((indexedMoon, _), idx) =>
          if (idx % 10000 == 0) {
            println(idx)
          }
          idx > 0 && indexedMoon == initialPosition
      }.take(1).toList.foreach {
        case ((_, _), idx) =>
          println(s"same initial planetary constellation occurred after $idx steps")

      }

    }
  }

}
