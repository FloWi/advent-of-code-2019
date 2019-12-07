package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.Day5._
import org.scalatest.{FunSuite, Matchers}

class Day5Spec extends FunSuite with Matchers {

  val input = getInput.unsafeRunSync()
  val ints  = input.split(",").toList.map(_.toInt)

  test("Part1 initialInstructionPointer 3 - storing value") {
    Day5.intCodeProgramWithInput("3,1,99", "4711") shouldBe "3,4711,99"
  }

  test("Part1 initialInstructionPointer 4 - output of a value") {
    Day5.intCodeProgramWithInputAndOutput("4,3,99,4711", "") shouldBe (("4711", "4,3,99,4711"))
  }

  test("Part1 - decodeInstruction: (normal) positional mode ") {
    Day5.decodeInstruction(getInts("2,4,3,4,33")) shouldBe Instruction(2, List(Positional(4), Positional(3), Positional(4)))
  }
  test("Part1 - decodeInstruction: immediate mode ") {
    Day5.decodeInstruction(getInts("1002,4,3,4,33")) shouldBe Instruction(2, List(Positional(4), Immediate(3), Positional(4)))
  }

  test("Part1 - example with immediate mode") {
    Day5.intCodeProgram("1002,4,3,4,33") shouldBe "1002,4,3,4,99"
  }

  test("Part1 - immediate mode must reverse parameters") {
    Day5.decodeInstruction(getInts("1101,1,238,225")) shouldBe Instruction(1, List(Immediate(1), Immediate(238), Positional(225)))
  }

  test("Part1 - outputting should be possible with immediate parameter ") {
    Day5.intCodeProgramWithInputAndOutput("104,0,99", "") shouldBe (("0", "104,0,99"))
  }

  test("Part1 initial") {
    Day5.intCodeProgram("1,9,10,3,2,3,11,0,99,30,40,50") shouldBe "3500,9,10,70,2,3,11,0,99,30,40,50"
  }
  test("Part1 ex1 (1 + 1 = 2)") {
    Day5.intCodeProgram("1,0,0,0,99") shouldBe "2,0,0,0,99"
  }
  test("Part1 ex2 (3 * 2 = 6)") {
    Day5.intCodeProgram("2,3,0,3,99") shouldBe "2,3,0,6,99"
  }
  test("Part1 ex3 (99 * 99 = 9801)") {
    Day5.intCodeProgram("2,4,4,5,99,0") shouldBe "2,4,4,5,99,9801"
  }
  test("Part1 ex4") {
    Day5.intCodeProgram("1,1,1,4,99,5,6,0,99") shouldBe "30,1,1,4,2,5,6,0,99"
  }

  test("Part2 jump-if-true") {
    //program will output 4711 if true
    Day5.intCodeProgramWithInputAndOutput("1105,1,4,99,104,4711,99", "") shouldBe ("4711", "1105,1,4,99,104,4711,99")
    Day5.intCodeProgramWithInputAndOutput("1105,0,4,99,104,4711,99", "") shouldBe ("", "1105,0,4,99,104,4711,99")
  }

  test("Part2 jump-if-false") {
    //program will output 4711 if true
    Day5.intCodeProgramWithInputAndOutput("1106,1,4,99,104,4711,99", "") shouldBe ("", "1106,1,4,99,104,4711,99")
    Day5.intCodeProgramWithInputAndOutput("1106,0,4,99,104,4711,99", "") shouldBe ("4711", "1106,0,4,99,104,4711,99")
  }

  test("Part2 less than") {
    //program will output 4711 if true
    Day5.intCodeProgramWithInputAndOutput("1107,0,1,5,99,666", "") shouldBe ("", "1107,0,1,5,99,1")
    Day5.intCodeProgramWithInputAndOutput("1107,0,0,5,99,666", "") shouldBe ("", "1107,0,0,5,99,0")
  }

  test("Part2 equals") {
    //program will output 4711 if true
    Day5.intCodeProgramWithInputAndOutput("1108,0,1,5,99,666", "") shouldBe ("", "1108,0,1,5,99,0")
    Day5.intCodeProgramWithInputAndOutput("1108,0,0,5,99,666", "") shouldBe ("", "1108,0,0,5,99,1")
  }

  test("Part2 example - position mode - equal to 8") {
    //Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)
    Day5.intCodeProgramWithInputAndOutput("3,9,8,9,10,9,4,9,99,-1,8", "8")._1 shouldBe "1"
    Day5.intCodeProgramWithInputAndOutput("3,9,8,9,10,9,4,9,99,-1,8", "666")._1 shouldBe "0"
  }

  test("Part2 example - position mode - less than 8") {
    //Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)
    Day5.intCodeProgramWithInputAndOutput("3,9,7,9,10,9,4,9,99,-1,8", "7")._1 shouldBe "1"
    Day5.intCodeProgramWithInputAndOutput("3,9,7,9,10,9,4,9,99,-1,8", "8")._1 shouldBe "0"
  }

  test("Part2 example - immediate mode - equal to 8") {
    // Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
    Day5.intCodeProgramWithInputAndOutput("3,3,1108,-1,8,3,4,3,99", "8")._1 shouldBe "1"
    Day5.intCodeProgramWithInputAndOutput("3,3,1108,-1,8,3,4,3,99", "666")._1 shouldBe "0"
  }

  test("Part2 example - immediate mode - less than 8") {
    //Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
    Day5.intCodeProgramWithInputAndOutput("3,3,1107,-1,8,3,4,3,99", "7")._1 shouldBe "1"
    Day5.intCodeProgramWithInputAndOutput("3,3,1107,-1,8,3,4,3,99", "8")._1 shouldBe "0"
  }

  test("Part2 large example") {
    //input instruction to ask for a single number. The program will then
    //output 999 if the input value is below 8,
    //output 1000 if the input value is equal to 8, or
    //output 1001 if the input value is greater than 8
    val program =
      "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    Day5.intCodeProgramWithInputAndOutput(program, "7")._1 shouldBe "999"
    Day5.intCodeProgramWithInputAndOutput(program, "8")._1 shouldBe "1000"
    Day5.intCodeProgramWithInputAndOutput(program, "9")._1 shouldBe "1001"
  }

  test("Part1 solution") {
    println(Day5.part1(input))
  }

  test("Part2 solution") {
    println(Day5.part2(input))
  }

}
