package de.flwi.adventofcode.v2019

import java.nio.file.Paths

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Day7 extends IOApp {

  case class Instruction(opcode: Int, parameters: Vector[Parameter])
  sealed trait Parameter
  case class Positional(pos: Int)  extends Parameter
  case class Immediate(value: Int) extends Parameter

  def intCodeProgramWithInput(inputLine: String, inputValues: String): String =
    intCodeProgram(getInts(inputLine), getInts(inputValues))._1.mkString(",")

  def intCodeProgramWithInputAndOutput(inputLine: String, inputValues: String): (String, String) = {
    val (currentProgramState, outputValues, _) = intCodeProgram(getInts(inputLine), getInts(inputValues))
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
    val result = amplifierProgram(getInts(inputLine))

    s"Highest signal from the amplifiers is: ${result._1}. Amplifier settings were ${result._2}"
  }

  def part2(inputLine: String): String = {
    val result = feedbackLoopAmplifierProgram(getInts(inputLine))

    s"Highest signal from the amplifiers is: ${result._1.outputValues.head}. Phase settings were ${result._2}"

  }
  def getInput: IO[String] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day7.txt"))
          .take(1)
      }
      .compile
      .toList
      .map(_.head)

  def getInts(inputLine: String): Vector[Int] =
    inputLine
      .split(",")
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toVector

  def calculateResultWithUpdatedNounAndVerb(
    ints: Vector[Int],
    noun: Int,
    verb: Int
  ): Vector[Int] =
    intCodeProgram(
      ints
        .updated(1, noun)
        .updated(2, verb),
      Vector.empty
    )._1

  def findNounAndVerbForSolution(
    ints: Vector[Int],
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

  def intCodeProgram(intProgram: Vector[Int], inputValues: Vector[Int], initialInstructionPointer: Int = 0): (Vector[Int], Vector[Int], Int) = {

    @scala.annotation.tailrec
    def helper(instructionPointer: Int, intProgram: Vector[Int], inputValues: Vector[Int], outputValues: Vector[Int]): (Vector[Int], Vector[Int], Int) = {

      myDebug()
      val instruction = decodeInstruction(intProgram, instructionPointer)

      def getValue(parameter: Parameter): Int =
        parameter match {
          case Positional(pos)  => intProgram(pos)
          case Immediate(value) => value
        }

      instruction match {
        case Instruction(1, Vector(first, second, Positional(positionOutput))) =>
          //Addition
          val result          = getValue(first) + getValue(second)
          val nextInstruction = instructionPointer + 4
          myDebug(
            s"${getValue(first)} + ${getValue(second)} = $result and stored it to register $positionOutput. Next instruction at index $nextInstruction (${intProgram(nextInstruction)})"
          )
          helper(nextInstruction, intProgram.updated(positionOutput, result), inputValues, outputValues)

        case Instruction(2, Vector(first, second, Positional(positionOutput))) =>
          //Multiplication
          val result          = getValue(first) * getValue(second)
          val nextInstruction = instructionPointer + 4
          myDebug(
            s"${getValue(first)} * ${getValue(second)} = $result and stored it to register $positionOutput. Next instruction at index $nextInstruction (${intProgram(nextInstruction)})"
          )
          helper(nextInstruction, intProgram.updated(positionOutput, result), inputValues, outputValues)

        case Instruction(3, Vector(Positional(positionInput))) =>
          //read input and stores it into register

          if (inputValues.isEmpty) {
            myDebug(s"no more input values - halting execution")
            (intProgram, outputValues, instructionPointer)
          }
          else {
            val currentInputValue = inputValues.head
            val restInputValues   = inputValues.tail

            val nextInstruction = instructionPointer + 2
            myDebug(
              s"read input ($currentInputValue) and stored it to register $positionInput. Next instruction at index $nextInstruction (${intProgram(nextInstruction)})"
            )
            helper(nextInstruction, intProgram.updated(positionInput, currentInputValue), restInputValues, outputValues)
          }
        case Instruction(4, Vector(first)) =>
          //read and outputs value from register or direct
          val nextInstruction = instructionPointer + 2

          val value = first match {
            case Positional(pos) =>
              val value = intProgram(pos)
              myDebug(s"outputting value $value (from register $pos). Next instruction at index $nextInstruction (${intProgram(nextInstruction)})")
              value
            case Immediate(value) =>
              myDebug(s"outputting value $value (immediate param). Next instruction at index $nextInstruction (${intProgram(nextInstruction)})")
              value
          }

          val newOutput = Vector(value) ++ outputValues
          println(s"amp #? - idx: $nextInstruction; Opcode: ${instruction.opcode}; outputting $newOutput")
          (intProgram, newOutput, nextInstruction)

        case Instruction(jumpIf, Vector(first, second)) if jumpIf == 5 || jumpIf == 6 =>
          val compareToZeroValue = getValue(first)
          val jumpParameterValue = getValue(second)

          val comparisonToZeroResult = compareToZeroValue == 0
          val result                 = if (jumpIf == 5) !comparisonToZeroResult else comparisonToZeroResult
          val name                   = if (jumpIf == 5) "jump-if-true" else "jump-if-false"

          val nextIndex = if (result) jumpParameterValue else instructionPointer + 3

          myDebug(
            s"$name: compareToZeroValue '$compareToZeroValue', result: $result. Next instruction at index $nextIndex. ${if (result) "Jump" else "No jump"}"
          )
          helper(nextIndex, intProgram, inputValues, outputValues)

        case Instruction(lessThanOrEqualsOpCode, Vector(compareFirst, compareSecond, Positional(output)))
            if lessThanOrEqualsOpCode == 7 || lessThanOrEqualsOpCode == 8 =>
          val left  = getValue(compareFirst)
          val right = getValue(compareSecond)

          val result             = if (lessThanOrEqualsOpCode == 7) left < right else left == right
          val comparisonOperator = if (lessThanOrEqualsOpCode == 7) "<" else "=="

          val resultValue = if (result) 1 else 0

          val nextIndex = instructionPointer + 4

          myDebug(s"$left $comparisonOperator $right = $result. Storing $resultValue at register $output")
          helper(nextIndex, intProgram.updated(output, resultValue), inputValues, outputValues)

        case Instruction(99, _) =>
          println(s"amp #? - idx: $instructionPointer; Opcode: ${instruction.opcode}; Done. Outputting ${outputValues.reverse}")
          (intProgram, outputValues.reverse, instructionPointer)

        case broken =>
          throw new RuntimeException(s"something went wrong: $broken")
      }

    }

    helper(initialInstructionPointer, intProgram, inputValues, Vector.empty)
  }

  def decodeInstruction(ints: Vector[Int], currentIndex: Int = 0): Instruction = {

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
        Instruction(opcode, 1.to(numberOfParameters).toVector.map(offset => Positional(ints(currentIndex + offset))))
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
        Instruction(opcode, parameters.toVector)
      }
    myDebug(s"decoded ${ints.slice(currentIndex, currentIndex + 6)} into $result")
    result
  }

  def intCodeProgramFromString(inputLine: String, inputs: String = ""): String =
    intCodeProgram(getInts(inputLine), getInts(inputs))._1.mkString(",")

  def amplifier(intcodeProgram: Vector[Int], amplifierSetting: Int, input: Int): Int =
    intCodeProgram(intcodeProgram, Vector(amplifierSetting, input))._2.head

  def amplifierProgram(intcodeProgram: Vector[Int]) = {
    val sequences = 0.to(4).toVector.permutations.map {
      case v @ Vector(a, b, c, d, e) =>
        val resA = intCodeProgram(intcodeProgram, Vector(a, 0))._2.head
        val resB = intCodeProgram(intcodeProgram, Vector(b, resA))._2.head
        val resC = intCodeProgram(intcodeProgram, Vector(c, resB))._2.head
        val resD = intCodeProgram(intcodeProgram, Vector(d, resC))._2.head
        val resE = intCodeProgram(intcodeProgram, Vector(e, resD))._2.head

        (resE, v)
    }

    val result = sequences.maxBy(_._1)
    result
  }

  case class Amplifier(prg: Vector[Int], phaseSetting: Int, pointer: Int, outputValues: Vector[Int] = Vector.empty) {

    val currentOpCode: Int = prg(pointer)

    def run(input: Int, iteration: Int): Amplifier = {

      val inputValues                                = if (iteration == 0) Vector(phaseSetting, input) else Vector(input)
      val (newPrg, outputValues, instructionPointer) = intCodeProgram(prg, inputValues, pointer)

      Amplifier(newPrg, phaseSetting, instructionPointer, outputValues)
    }
  }

  def runAmplifiers(intCodeProgram: Vector[Int], phaseSettings: Vector[Int]) = {

    def helper(amps: Vector[Amplifier], iteration: Int, inputForFirstAmp: Int): Vector[Amplifier] = {

      val amplifiers: ArrayBuffer[Amplifier] =
        collection.mutable.ArrayBuffer(amps: _*)

      def currentOpCodes = amplifiers.map(a => a.prg(a.pointer))

      amplifiers.indices.foreach { idx =>
        if (currentOpCodes.contains(99)) {
          //println(s"iteration: $iteration; Amp #$idx; Found an amp that has halted ${currentOpCodes}. Continuing computation")
        }
        val amp                = amplifiers(idx)
        val inputForCurrentAmp = if (idx == 0) inputForFirstAmp else amplifiers(idx - 1).outputValues.head

        val updatedAmp = amp.run(inputForCurrentAmp, iteration)
        amplifiers.update(idx, updatedAmp)
      //println(s"iteration: $iteration; Amp #$idx; input: $inputForCurrentAmp; result: ${updatedAmp.outputValues}. Current ip: ${updatedAmp.pointer} ")
      }

      val newAmps = amplifiers.toVector
      val last    = newAmps.last
      if (last.currentOpCode == 99)
        newAmps
      else
        helper(newAmps, iteration + 1, last.outputValues.head)
    }

    val amps = helper(phaseSettings.map(ps => Amplifier(intCodeProgram, ps, 0)), 0, 0)

    println(amps.mkString("\n"))
    amps.last
  }

  def feedbackLoopAmplifierProgram(prg: Vector[Int]) =
    9.to(5, step = -1)
      .toVector
      .permutations
      .map { phaseSettings =>
        val result = runAmplifiers(prg, phaseSettings)
        (result, phaseSettings)
      }
      .maxBy(_._1.outputValues.head)

  val isDebug = true
  def myDebug(x: Any): Unit =
    if (isDebug) { Console.println(x) }

}
