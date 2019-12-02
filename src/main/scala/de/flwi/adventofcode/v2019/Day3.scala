package de.flwi.adventofcode.v2019

import java.nio.file.Paths

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream

/*
=================
== PART 1
=================

 */

object Day3 extends IOApp {

  import FileReader._

  def run(args: List[String]): IO[ExitCode] =
    for {
      input <- getInput
      resultPart1 <- IO(part1(input))
      _ <- IO(println("result part 1"))
      _ <- IO(println(resultPart1))
      resultPart2 <- IO(part2(input))
      _ <- IO(println("result part 2"))
      _ <- IO(println(resultPart2))
      _ <- IO(
        println(
          s"result in the format aoc-website accepts ${resultPart2._1
            .formatted("%02d")}${resultPart2._2.formatted("%02d")}"
        )
      )
    } yield ExitCode.Success

  def part1(inputLine: String): String = {
    ???
  }

  def part2(inputLine: String): (Int, Int) = {
    ???
  }

  def getInput: IO[String] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day3.txt"))
          .take(1)
      }
      .compile
      .toList
      .map(_.head)

}
