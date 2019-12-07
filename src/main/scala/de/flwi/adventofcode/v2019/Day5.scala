package de.flwi.adventofcode.v2019

import java.nio.file.Paths

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream

object Day5 extends IOApp {

  case class Instruction(opcode: Int, parameters: List[Parameter])
  sealed trait Parameter
  case class Positional(pos: Int)  extends Parameter
  case class Immediate(value: Int) extends Parameter

  def intCodeProgramWithInput(inputLine: String, inputValues: String): String =
    intCodeProgram(getInts(inputLine), getInts(inputValues))._1.mkString(",")

  def intCodeProgramWithInputAndOutput(inputLine: String, inputValues: String): (String, String) = {
    val (currentProgramState, outputValues) = intCodeProgram(getInts(inputLine), getInts(inputValues))
    (outputValues.mkString(","), currentProgramState.mkString(","))
  }

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
    val actual                = Day5.intCodeProgramWithInputAndOutput(inputLine, "1")
    val oututs                = getInts(actual._1)
    val diagnosticCode :: Nil = oututs.filter(_ > 0)
    s"Diagnostic Code for part1 is: $diagnosticCode"
  }

  def part2(inputLine: String): String = {
    val actual                = Day5.intCodeProgramWithInputAndOutput(inputLine, "5")
    val oututs                = getInts(actual._1)
    val diagnosticCode :: Nil = oututs
    s"Diagnostic Code for part2 is: $diagnosticCode"

  }
  def getInput: IO[String] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day5.txt"))
          .take(1)
      }
      .compile
      .toList
      .map(_.head)

  def getInts(inputLine: String): List[Int] =
    inputLine
      .split(",")
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toList

  def calculateResultWithUpdatedNounAndVerb(
    ints: List[Int],
    noun: Int,
    verb: Int
  ): List[Int] =
    intCodeProgram(
      ints
        .updated(1, noun)
        .updated(2, verb),
      List.empty
    )._1

  def findNounAndVerbForSolution(
    ints: List[Int],
    expectedSolution: Int
  ): Option[(Int, Int)] = {
    val combinations = for {
      noun <- 0.to(99)
      verb <- 0.to(99)
    } yield (noun, verb)

    combinations.find {
      case (noun, verb) =>
        val result = calculateResultWithUpdatedNounAndVerb(ints, noun, verb)
        result.head == expectedSolution
    }
  }

  def intCodeProgram(intProgram: List[Int], inputValues: List[Int]): (List[Int], List[Int]) = {

    @scala.annotation.tailrec
    def helper(currentInstruction: Int, ints: List[Int], inputValues: List[Int], outputValues: List[Int]): (List[Int], List[Int]) = {

      myDebug()
      val instruction = decodeInstruction(ints, currentInstruction)

      def getValue(parameter: Parameter): Int =
        parameter match {
          case Positional(pos)  => ints(pos)
          case Immediate(value) => value
        }

      instruction match {
        case Instruction(1, first :: second :: Positional(positionOutput) :: Nil) =>
          //Addition
          val result          = getValue(first) + getValue(second)
          val nextInstruction = currentInstruction + 4
          myDebug(
            s"${getValue(first)} + ${getValue(second)} = $result and stored it to register $positionOutput. Next instruction at index $nextInstruction (${ints(nextInstruction)})"
          )
          helper(nextInstruction, ints.updated(positionOutput, result), inputValues, outputValues)

        case Instruction(2, first :: second :: Positional(positionOutput) :: Nil) =>
          //Multiplication
          val result          = getValue(first) * getValue(second)
          val nextInstruction = currentInstruction + 4
          myDebug(
            s"${getValue(first)} * ${getValue(second)} = $result and stored it to register $positionOutput. Next instruction at index $nextInstruction (${ints(nextInstruction)})"
          )
          helper(nextInstruction, ints.updated(positionOutput, result), inputValues, outputValues)

        case Instruction(3, Positional(positionInput) :: Nil) =>
          //read input and stores it into register
          val currentInputValue :: restInputValues = inputValues
          val nextInstruction                      = currentInstruction + 2
          myDebug(
            s"read input ($currentInputValue) and stored it to register $positionInput. Next instruction at index $nextInstruction (${ints(nextInstruction)})"
          )
          helper(nextInstruction, ints.updated(positionInput, currentInputValue), restInputValues, outputValues)

        case Instruction(4, first :: Nil) =>
          //read and outputs value from register or direct
          val nextInstruction = currentInstruction + 2

          val value = first match {
            case Positional(pos) =>
              val value = ints(pos)
              myDebug(s"outputting value $value (from register $pos). Next instruction at index $nextInstruction (${ints(nextInstruction)})")
              value
            case Immediate(value) =>
              myDebug(s"outputting value $value (immediate param). Next instruction at index $nextInstruction (${ints(nextInstruction)})")
              value
          }

          helper(nextInstruction, ints, inputValues, value :: outputValues)

        case Instruction(jumpIf, first :: second :: Nil) if jumpIf == 5 || jumpIf == 6 =>
          val compareToZeroValue = getValue(first)
          val jumpParameterValue = getValue(second)

          val comparisonToZeroResult = compareToZeroValue == 0
          val result                 = if (jumpIf == 5) !comparisonToZeroResult else comparisonToZeroResult
          val name                   = if (jumpIf == 5) "jump-if-true" else "jump-if-false"

          val nextIndex = if (result) jumpParameterValue else currentInstruction + 3

          myDebug(
            s"$name: compareToZeroValue '$compareToZeroValue', result: $result. Next instruction at index $nextIndex. ${if (result) "Jump" else "No jump"}"
          )
          helper(nextIndex, ints, inputValues, outputValues)

        case Instruction(lessThanOrEqualsOpCode, compareFirst :: compareSecond :: Positional(output) :: Nil)
            if lessThanOrEqualsOpCode == 7 || lessThanOrEqualsOpCode == 8 =>
          val left  = getValue(compareFirst)
          val right = getValue(compareSecond)

          val result             = if (lessThanOrEqualsOpCode == 7) left < right else left == right
          val comparisonOperator = if (lessThanOrEqualsOpCode == 7) "<" else "=="

          val resultValue = if (result) 1 else 0

          val nextIndex = currentInstruction + 4

          myDebug(s"$left $comparisonOperator $right = $result. Storing $resultValue at register $output")
          helper(nextIndex, ints.updated(output, resultValue), inputValues, outputValues)

        case Instruction(99, _) =>
          (ints, outputValues.reverse)

        case broken =>
          throw new RuntimeException(s"something went wrong: $broken")
      }

    }

    helper(0, intProgram, inputValues, List.empty)
  }

  def decodeInstruction(ints: List[Int], currentIndex: Int = 0): Instruction = {

    val wholeOpcode = ints(currentIndex)

    val isInImmediateMode = wholeOpcode >= 100

    val opcode = wholeOpcode % 100

    val numberOfParameters = opcode match {
      case 1  => 3 // Addition
      case 2  => 3 // Multiplication
      case 3  => 1 // write value
      case 4  => 1 // print value
      case 5  => 2 // jump if true
      case 6  => 2 // jump if false
      case 7  => 3 // less than
      case 8  => 3 // equals than
      case 99 => 0
    }

    val result =
      if (!isInImmediateMode) {
        Instruction(opcode, 1.to(numberOfParameters).toList.map(offset => Positional(ints(currentIndex + offset))))
      }
      else {
        /* the wholeOpcode is built like this
ABCDE
 1002

DE - two-digit initialInstructionPointer,      02 == initialInstructionPointer 2
 C - mode of 1st parameter,  0 == position mode
 B - mode of 2nd parameter,  1 == immediate mode
 A - mode of 3rd parameter,  0 == position mode,
                                  omitted due to being a leading zero
         */

        //take the non-initialInstructionPointer number (BC in the example) and format it with leading zeros (according to the number of expected parameters based on the initialInstructionPointer
        val parameterPart   = wholeOpcode / 100
        val formatString    = s"%0${numberOfParameters}d"
        val parameterString = formatString.format(parameterPart)

        val parameters = parameterString.reverse.zipWithIndex.map {
          case ('0', idx) => //position mode
            Positional(ints(1 + currentIndex + idx))

          case ('1', idx) =>
            Immediate(ints(1 + currentIndex + idx))

          case _ => throw new IllegalArgumentException(s"problem decoding ${ints.slice(currentIndex, currentIndex + 6)}")
        }
        Instruction(opcode, parameters.toList)
      }
    myDebug(s"decoded ${ints.slice(currentIndex, currentIndex + 6)} into $result")
    result
  }

  def intCodeProgram(inputLine: String, inputs: String = ""): String =
    intCodeProgram(getInts(inputLine), getInts(inputs))._1.mkString(",")

  val isDebug = false
  def myDebug(x: Any): Unit =
    if (isDebug) { Console.println(x) }

}
