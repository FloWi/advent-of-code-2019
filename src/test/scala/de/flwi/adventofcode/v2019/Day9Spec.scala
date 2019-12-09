package de.flwi.adventofcode.v2019

import java.nio.file.Paths

import de.flwi.adventofcode.v2019.Day9._
import org.scalatest.{FunSuite, Matchers}

class Day9Spec extends FunSuite with Matchers {

  val day7Input = getInput(Paths.get("data/day7.txt")).unsafeRunSync()
  val day7Ints  = day7Input.split(",").toList.map(_.toInt)

  val day7Part2Example1 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
  val day7Part2Example2 =
    "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"

  test("Day 7 part 2 - example #1 - permutation from example") {
//    amp #0 - Opcode: 4; next idx: 18; outputting 8726855
//    amp #1 - Opcode: 4; next idx: 18; outputting 17453714
//    amp #2 - Opcode: 4; next idx: 18; outputting 34907431
//    amp #3 - Opcode: 4; next idx: 18; outputting 69814864
//    amp #4 - Opcode: 4; next idx: 18; outputting 139629729
//    amp #0 - idx: 25; Opcode: 99; Done. Outputting 8726855
//    amp #1 - idx: 25; Opcode: 99; Done. Outputting 17453714
//    amp #2 - idx: 25; Opcode: 99; Done. Outputting 34907431
//    amp #3 - idx: 25; Opcode: 99; Done. Outputting 69814864
//    amp #4 - idx: 25; Opcode: 99; Done. Outputting 139629729
//    Part 2 - debugging - example 1: 139629729
    val actual = runAmplifiers(getInts(day7Part2Example1), Vector(9, 8, 7, 6, 5))
    actual.outputValues.head shouldBe 139629729
  }

  test("Day 7 part 2 - example #1 - different permutation") {
    //https://www.reddit.com/r/adventofcode/comments/e7eezs/day_7_part_2_implementation_struggles/
    val actual = runAmplifiers(getInts("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"), Vector(5, 6, 7, 8, 9))
    actual.outputValues.head shouldBe 61696857
  }

  test("Day 7 part 2 - example #1 - debugging amp #0") {
    val ampInitial = Amplifier(IntcodeState(getInts(day7Part2Example1), 0, Vector.empty, Vector.empty, ""), phaseSetting = 9)

    val ampAfterIteration0 = ampInitial.run(0, 0)
    ampAfterIteration0.outputValues shouldBe Vector(5)
    ampAfterIteration0.intcodeState.intProgram shouldBe Vector(3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005,
      28, 6, 99, 5, 5, 5)
    ampAfterIteration0.intcodeState.instructionPointer shouldBe 18

    val ampAfterIteration1 = ampAfterIteration0.run(129, 1) //from Amp E
    ampAfterIteration1.outputValues.head shouldBe 263
  }

  test("Day 7 part 2 - example #1") {
    val (amplifier, phaseSettings) = feedbackLoopAmplifierProgram(getInts(day7Part2Example1))
    (amplifier.outputValues.head, phaseSettings) shouldBe (139629729, Vector(9, 8, 7, 6, 5))
  }

  test("Day 7 part 2 - example #2") {
//    amp #0 - Opcode: 4; next idx: 44; outputting 18206
//    amp #1 - Opcode: 4; next idx: 44; outputting 18209
//    amp #2 - Opcode: 4; next idx: 44; outputting 18213
//    amp #3 - Opcode: 4; next idx: 44; outputting 18214
//    amp #4 - Opcode: 4; next idx: 44; outputting 18216
//    amp #0 - idx: 51; Opcode: 99; Done. Outputting 18206
//    amp #1 - idx: 51; Opcode: 99; Done. Outputting 18209
//    amp #2 - idx: 51; Opcode: 99; Done. Outputting 18213
//    amp #3 - idx: 51; Opcode: 99; Done. Outputting 18214
//    amp #4 - idx: 51; Opcode: 99; Done. Outputting 18216
    val (amplifier, phaseSettings) = feedbackLoopAmplifierProgram(getInts(day7Part2Example2))
    (amplifier.outputValues.head, phaseSettings) shouldBe (18216, Vector(9, 7, 8, 5, 6))
  }

  test("Day 7 part 2 - debugging example #1") {
    val actual = runAmplifiers(getInts(day7Part2Example2), Vector(9, 7, 8, 5, 6))
    actual.outputValues.head shouldBe 18216
  }

  test("Day 7 part 2 - debugging real input") {
//    amp #0 - idx: 429; Opcode: 4; next idx: 429; outputting 8759949
//    amp #1 - idx: 510; Opcode: 4; next idx: 510; outputting 17519898
//    amp #2 - idx: 348; Opcode: 4; next idx: 348; outputting 17519900
//    amp #3 - idx: 267; Opcode: 4; next idx: 267; outputting 17519902
//    amp #4 - idx: 186; Opcode: 4; next idx: 186; outputting 17519904
//    amp #0 - idx: 429; Opcode: 99; Done. Outputting 8759949
//    amp #1 - idx: 510; Opcode: 99; Done. Outputting 17519898
//    amp #2 - idx: 348; Opcode: 99; Done. Outputting 17519900
//    amp #3 - idx: 267; Opcode: 99; Done. Outputting 17519902
//    amp #4 - idx: 186; Opcode: 99; Done. Outputting 17519904

    val actual = runAmplifiers(getInts(day7Input), Vector(8, 9, 7, 6, 5))
    print(s"output of last amp: ${actual.outputValues}")
    actual.outputValues.head shouldBe 17519904
  }

  test("Day 7 part 1") {
    println(part1(day7Input))
  }

  test("Day 7 part 2") {
    println(part2(day7Input))
  }

  test("Day 7 part 1 - example #1") {
    amplifierProgram(getInts("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")) shouldBe (43210, Vector(4, 3, 2, 1, 0))
  }

  test("Day 7 part 1 - example #2") {
    amplifierProgram(getInts("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")) shouldBe (54321, Vector(0, 1, 2, 3, 4))
  }

  test("Day 7 part 1 - example #3") {
    amplifierProgram(getInts("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")) shouldBe (65210, Vector(
      1,
      0,
      4,
      3,
      2
    ))
  }

  test("Day 7 - Original int-code-machine part1 - initialInstructionPointer 3 - storing value") {
    Day9.intCodeProgramWithInput("3,1,99", "4711").intProgram shouldBe getInts("3,4711,99")
  }

  test("Day 7 - Original int-code-machine part1 - initialInstructionPointer 4 - output of a value") {
    Day9.intCodeProgramWithInputAndOutput("4,3,99,4711", "") shouldBe (("4711", "4,3,99,4711"))
  }

  test("Day 7 - Original int-code-machine part1 - decodeInstruction: (normal) positional mode ") {
    Day9.decodeInstruction(getInts("2,4,3,4,33")) shouldBe Instruction(OpCode.Multiplication, Vector(Positional(4), Positional(3), Positional(4)))
  }
  test("Day 7 - Original int-code-machine part1 - decodeInstruction: immediate mode ") {
    Day9.decodeInstruction(getInts("1002,4,3,4,33")) shouldBe Instruction(OpCode.Multiplication, Vector(Positional(4), Immediate(3), Positional(4)))
  }

  test("Day 7 - Original int-code-machine part1 - example with immediate mode") {
    Day9.intCodeProgramFromString("1002,4,3,4,33") shouldBe "1002,4,3,4,99"
  }

  test("Day 7 - Original int-code-machine part1 - immediate mode must reverse parameters") {
    Day9.decodeInstruction(getInts("1101,1,238,225")) shouldBe Instruction(OpCode.Addition, Vector(Immediate(1), Immediate(238), Positional(225)))
  }

  test("Day 7 - Original int-code-machine part1 - outputting should be possible with immediate parameter ") {
    Day9.intCodeProgramWithInputAndOutput("104,0,99", "") shouldBe (("0", "104,0,99"))
  }

  test("Day 7 - Original int-code-machine part1 - initial") {
    Day9.intCodeProgramFromString("1,9,10,3,2,3,11,0,99,30,40,50") shouldBe "3500,9,10,70,2,3,11,0,99,30,40,50"
  }
  test("Day 7 - Original int-code-machine part1 - ex1 (1 + 1 = 2)") {
    Day9.intCodeProgramFromString("1,0,0,0,99") shouldBe "2,0,0,0,99"
  }
  test("Day 7 - Original int-code-machine part1 - ex2 (3 * 2 = 6)") {
    Day9.intCodeProgramFromString("2,3,0,3,99") shouldBe "2,3,0,6,99"
  }
  test("Day 7 - Original int-code-machine part1 - ex3 (99 * 99 = 9801)") {
    Day9.intCodeProgramFromString("2,4,4,5,99,0") shouldBe "2,4,4,5,99,9801"
  }
  test("Day 7 - Original int-code-machine part1 - ex4") {
    Day9.intCodeProgramFromString("1,1,1,4,99,5,6,0,99") shouldBe "30,1,1,4,2,5,6,0,99"
  }

  test("Day 7 - Original int-code-machine part2 - jump-if-true") {
    //program will output 4711 if true
    Day9.intCodeProgramWithInputAndOutput("1105,1,4,99,104,4711,99", "") shouldBe ("4711", "1105,1,4,99,104,4711,99")
    Day9.intCodeProgramWithInputAndOutput("1105,0,4,99,104,4711,99", "") shouldBe ("", "1105,0,4,99,104,4711,99")
  }

  test("Day 7 - Original int-code-machine part2 - jump-if-false") {
    //program will output 4711 if true
    Day9.intCodeProgramWithInputAndOutput("1106,1,4,99,104,4711,99", "") shouldBe ("", "1106,1,4,99,104,4711,99")
    Day9.intCodeProgramWithInputAndOutput("1106,0,4,99,104,4711,99", "") shouldBe ("4711", "1106,0,4,99,104,4711,99")
  }

  test("Day 7 - Original int-code-machine part2 - less than") {
    //program will output 4711 if true
    Day9.intCodeProgramWithInputAndOutput("1107,0,1,5,99,666", "") shouldBe ("", "1107,0,1,5,99,1")
    Day9.intCodeProgramWithInputAndOutput("1107,0,0,5,99,666", "") shouldBe ("", "1107,0,0,5,99,0")
  }

  test("Day 7 - Original int-code-machine part2 - equals") {
    //program will output 4711 if true
    Day9.intCodeProgramWithInputAndOutput("1108,0,1,5,99,666", "") shouldBe ("", "1108,0,1,5,99,0")
    Day9.intCodeProgramWithInputAndOutput("1108,0,0,5,99,666", "") shouldBe ("", "1108,0,0,5,99,1")
  }

  test("Day 7 - Original int-code-machine part2 - example - position mode - equal to 8") {
    //Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)
    Day9.intCodeProgramWithInputAndOutput("3,9,8,9,10,9,4,9,99,-1,8", "8")._1 shouldBe "1"
    Day9.intCodeProgramWithInputAndOutput("3,9,8,9,10,9,4,9,99,-1,8", "666")._1 shouldBe "0"
  }

  test("Day 7 - Original int-code-machine part2 - example - position mode - less than 8") {
    //Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)
    Day9.intCodeProgramWithInputAndOutput("3,9,7,9,10,9,4,9,99,-1,8", "7")._1 shouldBe "1"
    Day9.intCodeProgramWithInputAndOutput("3,9,7,9,10,9,4,9,99,-1,8", "8")._1 shouldBe "0"
  }

  test("Day 7 - Original int-code-machine part2 - example - immediate mode - equal to 8") {
    // Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
    Day9.intCodeProgramWithInputAndOutput("3,3,1108,-1,8,3,4,3,99", "8")._1 shouldBe "1"
    Day9.intCodeProgramWithInputAndOutput("3,3,1108,-1,8,3,4,3,99", "666")._1 shouldBe "0"
  }

  test("Day 7 - Original int-code-machine part2 - example - immediate mode - less than 8") {
    //Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
    Day9.intCodeProgramWithInputAndOutput("3,3,1107,-1,8,3,4,3,99", "7")._1 shouldBe "1"
    Day9.intCodeProgramWithInputAndOutput("3,3,1107,-1,8,3,4,3,99", "8")._1 shouldBe "0"
  }

}
