package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.Instruction.decodeInstruction
import de.flwi.adventofcode.v2019.IntCodeComputer.myDebug

sealed trait OpCode
object OpCode {
  def fromInt(opcode: Long): OpCode =
    opcode match {
      case 1  => Addition
      case 2  => Multiplication
      case 3  => WriteValue
      case 4  => PrintValue
      case 5  => JumpIfTrue
      case 6  => JumpIfFalse
      case 7  => LessThan
      case 8  => Equals
      case 9  => AdjustRelativeBase
      case 99 => Finished
    }

  def numberOfParamters(opCode: OpCode): Int = opCode match {
    case Addition           => 3
    case Multiplication     => 3
    case WriteValue         => 1
    case PrintValue         => 1
    case JumpIfTrue         => 2
    case JumpIfFalse        => 2
    case LessThan           => 3
    case Equals             => 3
    case AdjustRelativeBase => 1
    case Finished           => 0
  }

  case object Addition extends OpCode

  case object Multiplication extends OpCode

  case object WriteValue extends OpCode

  case object PrintValue extends OpCode

  case object JumpIfTrue extends OpCode

  case object JumpIfFalse extends OpCode

  case object LessThan extends OpCode

  case object Equals extends OpCode

  case object AdjustRelativeBase extends OpCode

  case object Finished extends OpCode

}

object Instruction {

  def decodeInstruction(ints: Vector[Long], currentIndex: Int): Instruction =
    decodeInstruction(ints.zipWithIndex.map(_.swap).toMap, currentIndex)

  def decodeInstruction(ints: Map[Int, Long], currentIndex: Int): Instruction = {

    import Parameter._

    val wholeOpcode = ints(currentIndex)

    val isInImmediateMode  = wholeOpcode >= 100
    val opcode             = OpCode.fromInt(wholeOpcode % 100)
    val numberOfParameters = OpCode.numberOfParamters(opcode)

    val result =
      if (!isInImmediateMode) {
        Instruction(opcode, 1.to(numberOfParameters).toVector.map(offset => Positional(ints(currentIndex + offset).toInt)))
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
            Positional(ints(1 + currentIndex + idx).toInt)

          case ('1', idx) =>
            Immediate(ints(1 + currentIndex + idx))

          case ('2', idx) =>
            val offset = ints(1 + currentIndex + idx)
            Relative(offset.toInt)

          case _ => throw new IllegalArgumentException(s"problem decoding ${ints.slice(currentIndex, currentIndex + 6)}")
        }
        Instruction(opcode, parameters.toVector)
      }
    myDebug(s"decoded ${ints.slice(currentIndex, currentIndex + 6)} into $result")
    result
  }

}

case class Instruction(opcode: OpCode, parameters: Vector[Parameter])
sealed trait Parameter
object Parameter {

  case class Positional(pos: Int) extends Parameter

  case class Immediate(value: Long) extends Parameter

  case class Relative(offsetFromRelativeBase: Int) extends Parameter

  def getValueOfParameter(parameter: Parameter, intProgram: Vector[Long], relativeBase: Int): Long =
    getValueOfParameter(parameter, intProgram.zipWithIndex.map(_.swap).toMap, relativeBase)

  def getValueOfParameter(parameter: Parameter, intProgram: Map[Int, Long], relativeBase: Int): Long =
    parameter match {
      case Positional(pos)  => intProgram(pos)
      case Immediate(value) => value
      case Relative(offset) => intProgram(relativeBase + offset)
    }

}

object IntCodeComputer {

  val isDebug = false
  def myDebug(x: Any): Unit =
    if (isDebug) {
      Console.println(x)
    }

  def intCodeProgram(intProgram: Vector[Long], inputValues: Vector[Long], initialInstructionPointer: Int, id: String): IntCodeComputer =
    IntCodeComputer(intProgram, initialInstructionPointer, inputValues, Vector.empty, 0, id)

  def intCodeProgram(intcodeState: IntCodeComputer): IntCodeComputer =
    intcodeState.run()

  def calculateResultWithUpdatedNounAndVerb(
    ints: Vector[Long],
    noun: Long,
    verb: Long
  ): Vector[Long] =
    intCodeProgram(
      ints
        .updated(1, noun)
        .updated(2, verb),
      Vector.empty,
      0,
      ""
    ).outputValues

  def findNounAndVerbForSolution(
    ints: Vector[Long],
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

  def intCodeProgramWithInput(inputLine: String, inputValues: String): IntCodeComputer =
    intCodeProgram(getInts(inputLine), getInts(inputValues), 0, "").run()

  def intCodeProgramWithInputAndOutput(inputLine: String, inputValues: String): (String, String) = {
    val current = intCodeProgram(getInts(inputLine), getInts(inputValues), 0, "").run()
    (current.outputValues.mkString(","), current.ints.mkString(","))
  }

  def getInts(inputLine: String): Vector[Long] =
    inputLine
      .split(",")
      .filter(_.nonEmpty)
      .map(_.toLong)
      .toVector

  def fromString(inputLine: String, inputs: String = ""): IntCodeComputer =
    IntCodeComputer(getInts(inputLine), 0, getInts(inputs), Vector.empty, 0, "")

  def apply(intProgram: Vector[Long],
            instructionPointer: Int,
            inputValues: Vector[Long],
            outputValues: Vector[Long],
            relativeBase: Int,
            id: String): IntCodeComputer = {
    val prg = intProgram.zipWithIndex.map(_.swap).toMap.withDefaultValue(0L)

    new IntCodeComputer(
      prg,
      instructionPointer,
      inputValues,
      outputValues,
      relativeBase,
      id
    )

  }
}

case class IntCodeComputer private (intProgram: Map[Int, Long],
                                    instructionPointer: Int,
                                    inputValues: Vector[Long],
                                    outputValues: Vector[Long],
                                    relativeBase: Int,
                                    id: String) {

  lazy val ints: Vector[Long] = {
    val maxIndex = intProgram.keys.max
    Vector.tabulate(maxIndex + 1)(intProgram.apply)
  }

  def run(inputValues: Vector[Long]): IntCodeComputer =
    this.copy(inputValues = inputValues).run()

  @scala.annotation.tailrec
  final def run(): IntCodeComputer = {

    //instructionPointer: Int, intProgram: Vector[Int], inputValues: Vector[Int], outputValues: Vector[Int]

    import Parameter._
    myDebug()
    val instruction = decodeInstruction(intProgram, instructionPointer)

    def getPositionOutput(parameter: Parameter) = parameter match {
      case Positional(pos)                  => pos
      case Immediate(_)                     => throw new IllegalStateException("Output param can't be in immediate mode")
      case Relative(offsetFromRelativeBase) => relativeBase + offsetFromRelativeBase
    }

    def getValue(parameter: Parameter): Long =
      getValueOfParameter(parameter, intProgram, relativeBase)

    instruction match {
      case Instruction(OpCode.Addition, Vector(first, second, parameter: Parameter)) =>
        val positionOutput = getPositionOutput(parameter)
        //Addition
        val result          = getValue(first) + getValue(second)
        val nextInstruction = instructionPointer + 4
        myDebug(
          s"${getValue(first)} + ${getValue(second)} = $result and stored it to register $positionOutput. Next instruction at index $nextInstruction (${intProgram(nextInstruction)})"
        )
        IntCodeComputer(intProgram.updated(positionOutput, result), nextInstruction, inputValues, outputValues, relativeBase, id).run()

      case Instruction(OpCode.Multiplication, Vector(first, second, parameter: Parameter)) =>
        val positionOutput = getPositionOutput(parameter)

        //Multiplication
        val result          = getValue(first) * getValue(second)
        val nextInstruction = instructionPointer + 4
        myDebug(
          s"${getValue(first)} * ${getValue(second)} = $result and stored it to register $positionOutput. Next instruction at index $nextInstruction (${intProgram(nextInstruction)})"
        )
        IntCodeComputer(intProgram.updated(positionOutput, result), nextInstruction, inputValues, outputValues, relativeBase, id).run()

      case Instruction(OpCode.WriteValue, Vector(parameter)) =>
        //read input and stores it into register

        val positionOutput = getPositionOutput(parameter)

        if (inputValues.isEmpty) {
          myDebug(s"no more input values - halting execution")
          IntCodeComputer(intProgram, instructionPointer, inputValues, outputValues, relativeBase, id)
        }
        else {
          val currentInputValue = inputValues.head
          val restInputValues   = inputValues.tail

          val nextInstruction = instructionPointer + 2
          myDebug(
            s"read input ($currentInputValue) and stored it to register $positionOutput. Next instruction at index $nextInstruction (${intProgram(nextInstruction)})"
          )
          IntCodeComputer(intProgram.updated(positionOutput, currentInputValue), nextInstruction, restInputValues, outputValues, relativeBase, id).run()
        }
      case Instruction(OpCode.PrintValue, Vector(first)) =>
        //read and outputs value from register or direct
        val nextInstruction = instructionPointer + 2

        val value = getValue(first)
        myDebug(s"outputting value $value (parameter was: $first). Next instruction at index $nextInstruction (${intProgram(nextInstruction)})")

        val newOutput = Vector(value) ++ outputValues
        myDebug(s"amp #$id - idx: $nextInstruction; Opcode: ${instruction.opcode}; outputting $newOutput")
        IntCodeComputer(intProgram, nextInstruction, inputValues, newOutput, relativeBase, id)

      case Instruction(jumpIf, Vector(first, second)) if jumpIf == OpCode.JumpIfTrue || jumpIf == OpCode.JumpIfFalse =>
        val compareToZeroValue = getValue(first)
        val jumpParameterValue = getValue(second)

        val comparisonToZeroResult = compareToZeroValue == 0
        val result                 = if (jumpIf == OpCode.JumpIfTrue) !comparisonToZeroResult else comparisonToZeroResult

        val nextIndex = if (result) jumpParameterValue else instructionPointer + 3

        myDebug(
          s"$jumpIf: compareToZeroValue '$compareToZeroValue', result: $result. Next instruction at index $nextIndex. ${if (result) "Jump" else "No jump"}"
        )
        IntCodeComputer(intProgram, nextIndex.toInt, inputValues, outputValues, relativeBase, id).run()

      case Instruction(lessThanOrEqualsOpCode, Vector(compareFirst, compareSecond, parameter: Parameter))
          if lessThanOrEqualsOpCode == OpCode.LessThan || lessThanOrEqualsOpCode == OpCode.Equals =>
        val positionOutput = getPositionOutput(parameter)

        val left  = getValue(compareFirst)
        val right = getValue(compareSecond)

        val result             = if (lessThanOrEqualsOpCode == OpCode.LessThan) left < right else left == right
        val comparisonOperator = if (lessThanOrEqualsOpCode == OpCode.LessThan) "<" else "=="

        val resultValue = if (result) 1L else 0L

        val nextIndex = instructionPointer + 4

        myDebug(s"$left $comparisonOperator $right = $result. Storing $resultValue at register $positionOutput")

        IntCodeComputer(intProgram.updated(positionOutput, resultValue), nextIndex, inputValues, outputValues, relativeBase, id).run()

      case Instruction(OpCode.AdjustRelativeBase, Vector(parameter)) =>
        val offset          = getValue(parameter)
        val newRelativeBase = relativeBase + offset
        IntCodeComputer(intProgram, instructionPointer + 2, inputValues, outputValues, newRelativeBase.toInt, id).run()

      case Instruction(OpCode.Finished, _) =>
        myDebug(s"amp #$id - idx: $instructionPointer; Opcode: ${instruction.opcode}; Done. Outputting ${outputValues.reverse}")
        IntCodeComputer(intProgram, instructionPointer, inputValues, outputValues, relativeBase, id)

      case _ =>
        throw new IllegalArgumentException(s"broken\ncurrent instruction: $instruction\n$this")

    }
  }

  def opCode: Long = intProgram(instructionPointer)
}
