package de.flwi.adventofcode.v2019.day14

import java.nio.file.{Path, Paths}

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import de.flwi.adventofcode.v2019.FileReader.lines
import de.flwi.adventofcode.v2019.day14.Model.{calculateOreAmountForOneFuel, createRecipeBook, parseInput}
import fs2.Stream

object Day14 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      input       <- getInput
      resultPart1 <- IO(part1(input))
      _           <- IO(println("result part 1"))
      _           <- IO(println(resultPart1))
      resultPart2 <- IO(part2(input))
      _           <- IO(println("result part 2"))
      _           <- IO(println(resultPart2))
    } yield ExitCode.Success

  def part1(input: List[String]): String = {
    val recipeBook = createRecipeBook(parseInput(input))
    val result     = calculateOreAmountForOneFuel(recipeBook)

    s"$result ORE needed to produce 1 FUEL"
  }

  def part2(input: List[String]): String =
    ???

  def getInput: IO[List[String]] =
    getInput(Paths.get("data/day14.txt"))

  def getInput(filePath: Path): IO[List[String]] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, filePath)
      }
      .compile
      .toList

  val isDebug = false
  def myDebug(x: Any): Unit =
    if (isDebug) {
      Console.println(x)
    }
}
