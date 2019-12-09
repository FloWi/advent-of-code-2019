package de.flwi.adventofcode.v2019

import java.nio.file.{Path, Paths}

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import de.flwi.adventofcode.v2019.IntCodeComputer.getInts
import fs2.Stream


object Day9 extends IOApp {

  import FileReader._

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

  def part1(inputLine: String): String = {
    val initial = IntCodeComputer(getInts(inputLine), instructionPointer = 0, inputValues = Vector(1L), outputValues = Vector.empty, relativeBase = 0, id = "")
    val actual  = initial.run()

    actual.outputValues.toString()
  }

  def part2(inputLine: String): String = {
    val initial = IntCodeComputer(getInts(inputLine), instructionPointer = 0, inputValues = Vector(2L), outputValues = Vector.empty, relativeBase = 0, id = "")
    val actual  = initial.run()

    actual.outputValues.toString()
  }

  def getInput: IO[String] =
    getInput(Paths.get("data/day9.txt"))

  def getInput(filePath: Path): IO[String] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, filePath)
          .take(1)
      }
      .compile
      .toList
      .map(_.head)


  def runIntCodeProgram(intProgram: Vector[Long], inputValues: Vector[Long], initialInstructionPointer: Int, id: String): IntCodeComputer =
      IntCodeComputer(
        intProgram,
        instructionPointer = initialInstructionPointer,
        inputValues = inputValues,
        outputValues = Vector.empty,
        relativeBase = 0,
        id = id
      ).run()


  def amplifierProgram(intcodeProgram: Vector[Long]) = {
    val sequences = 0.to(4).toVector.permutations.map {
      case v @ Vector(a, b, c, d, e) =>
        val resA = runIntCodeProgram(intcodeProgram, Vector(a, 0), 0, "a").outputValues.head
        val resB = runIntCodeProgram(intcodeProgram, Vector(b, resA), 0, "b").outputValues.head
        val resC = runIntCodeProgram(intcodeProgram, Vector(c, resB), 0, "c").outputValues.head
        val resD = runIntCodeProgram(intcodeProgram, Vector(d, resC), 0, "d").outputValues.head
        val resE = runIntCodeProgram(intcodeProgram, Vector(e, resD), 0, "e").outputValues.head

        (resE, v)
    }

    val result = sequences.maxBy(_._1)
    result
  }

  val isDebug = false
  def myDebug(x: Any): Unit =
    if (isDebug) {
      Console.println(x)
    }
}
