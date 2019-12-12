package de.flwi.adventofcode.v2019

import java.awt.image.BufferedImage
import java.io.File

import de.flwi.adventofcode.v2019.Day11.{getInput, solve, Color, Direction}
import javax.imageio.ImageIO
import org.scalatest.{FunSpec, Matchers}

class Day11Spec extends FunSpec with Matchers {

  describe("Part 1") {
    it("should solve part1") {
      val res = for {
        input  <- getInput
        result <- Day11.part1(input.mkString("\n"))
      } yield result

      println(res.unsafeRunSync())

      describe("Part 2") {

        it("should solve part2") {
          val res = for {
            input  <- getInput
            result <- Day11.part2(input.mkString("\n"))
          } yield result

          println(res.unsafeRunSync())

        }
      }

    }
  }
}
