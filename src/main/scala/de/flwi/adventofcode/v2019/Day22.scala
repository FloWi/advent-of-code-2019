package de.flwi.adventofcode.v2019

import java.nio.file.{Path, Paths}

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream

object Day22 extends IOApp {
  def findOriginIndex(stackSize: Long, instructions: List[ShuffleInstruction], indexOfInterest: Long) =
    0L

  def findOriginIndex(stackSize: Long, instruction: ShuffleInstruction, indexOfInterest: Long): Long =
    instruction match {
      case DealIntoNewStack =>
        stackSize - 1 - indexOfInterest

      case DealWithIncrement(increment) =>
        // example with increment 3
        //0 1 2 3 4 5 6 7 8 9
        //0 7 4 1 8 5 2 9 6 3

        //val newIndex = idx * increment % stackSize
        indexOfInterest * stackSize / increment

      case Cut(n) =>
        0L
    }

  def applyInstruction(stack: Vector[Int], instruction: ShuffleInstruction): Vector[Int] =
    instruction match {
      case DealIntoNewStack             => dealIntoNewStack(stack)
      case DealWithIncrement(increment) => dealWithIncrement(stack, increment)
      case Cut(n)                       => cut(stack, n)
    }

  def applyInstructionSet(stack: Vector[Int], instructions: List[String]): Vector[Int] =
    instructions
      .map(parseInstruction)
      .foldLeft(stack) {
        case (stack, instruction) =>
          applyInstruction(stack, instruction)
      }

  def applyInstructionSetNTimes(stackSize: Long, indexOfInterest: Long, numberOfIterations: Long): Long =
    0L

  def cut(stack: Vector[Int], n: Int): Vector[Int] = {
    val cutIdx =
      if (n < 0)
        stack.length + n
      else n

    val (head, tail) = stack.splitAt(cutIdx)
    tail ++ head
  }

  sealed trait ShuffleInstruction
  case object DealIntoNewStack                 extends ShuffleInstruction
  case class DealWithIncrement(increment: Int) extends ShuffleInstruction
  case class Cut(n: Int)                       extends ShuffleInstruction

  def parseInstruction(line: String): ShuffleInstruction =
//      deal with increment 64
//      deal into new stack
//      cut 1004
    if (line == "deal into new stack")
      DealIntoNewStack
    else if (line.startsWith("deal with"))
      DealWithIncrement(line.split(" ").last.toInt)
    else
      Cut(line.split(" ").last.toInt)

  def dealWithIncrement(stack: Vector[Int], increment: Int): Vector[Int] = {
    val result = collection.mutable.ArrayBuffer.fill(stack.size)(0)

    stack.zipWithIndex.foreach { case (value, idx) => result.update(idx * increment % stack.length, value) }

    result.toVector
  }

  def dealIntoNewStack(stack: Vector[Int]): Vector[Int] = stack.reverse

  def generateStack(stackSize: Int): Vector[Int] = Vector.tabulate(stackSize)(identity)

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

  def part1(input: List[String]): String = {
    //After shuffling your factory order deck of 10007 cards, what is the position of card 2019?

    val result = applyInstructionSet(generateStack(10007), input)

    s"The position of the 2019 card is ${result.indexOf(2019)}"
  }

  def part2(input: List[String]): String = {
    val result = applyInstructionSetNTimes(119315717514047L, 2020L, 101741582076661L)
    s"The number of the card at position 2020 is $result"
  }

  def getInput: IO[List[String]] =
    getInput(Paths.get("data/day22.txt"))

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
