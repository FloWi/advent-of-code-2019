package de.flwi.adventofcode.v2019

import java.nio.file.Paths

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import fs2.Stream

/*
=================
== PART 1
=================

An Intcode program is a list of integers separated by commas
(like 1,0,0,3,99).

To run one, start by looking at the first integer (called position 0).
Here, you will find an opcode - either 1, 2, or 99.
The opcode indicates what to do; for example, 99 means that
the program is finished and should immediately halt.
Encountering an unknown opcode means something went wrong.


Opcode 1 adds together numbers read from two positions and
stores the result in a third position.
The three integers immediately after the opcode tell you these
three positions - the first two indicate the positions from which you
should read the input values, and the third indicates the position
at which the output should be stored.


For example, if your Intcode computer encounters 1,10,20,30,
it should read the values at positions 10 and 20,
add those values, and then overwrite the value at position 30 with their sum.

Opcode 2 works exactly like opcode 1, except it multiplies the
two inputs instead of adding them.
Again, the three integers after the opcode indicate
where the inputs and outputs are, not their values.

Once you're done processing an opcode, move to the next one
by stepping forward 4 positions.


For example, suppose you have the following program:

1,9,10,3,2,3,11,0,99,30,40,50
For the purposes of illustration, here is the same program split
into multiple lines:

1,9,10,3,
2,3,11,0,
99,
30,40,50


The first four integers, 1,9,10,3, are at positions 0, 1, 2, and 3.
Together, they represent the first opcode (1, addition),
the positions of the two inputs (9 and 10),
and the position of the output (3).
To handle this opcode, you first need to get the values at the
input positions: position 9 contains 30, and position 10 contains 40.
Add these numbers together to get 70.
Then, store this value at the output position;
here, the output position (3) is at position 3, so it overwrites itself.
Afterward, the program looks like this:

1,9,10,70,
2,3,11,0,
99,
30,40,50
Step forward 4 positions to reach the next opcode, 2.
This opcode works just like the previous,
but it multiplies instead of adding.
The inputs are at positions 3 and 11;
these positions contain 70 and 50 respectively.
Multiplying these produces 3500; this is stored at position 0:

3500,9,10,70,
2,3,11,0,
99,
30,40,50
Stepping forward 4 more positions arrives at opcode 99,
halting the program.

 */

object Day2 extends IOApp {

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
    /*
    Once you have a working computer, the first step is to
    restore the gravity assist program (your puzzle input)
    to the "1202 program alarm" state it had just before
    the last computer caught fire.

    To do this, before running the program,
    replace position 1 with the value 12 and replace position 2
    with the value 2.

    What value is left at position 0 after the program halts?
     */

    //    .map(result => s"Solution: ${result.split(",").head}\n${result}")

    calculateResultWithUpdatedNounAndVerb(getInts(inputLine), 12, 2)
      .mkString(",")
  }

  def part2(inputLine: String): (Int, Int) = {

    /*
    you need to determine what pair of inputs produces the output 19690720."
    The inputs should still be provided to the program
    by replacing the values at addresses 1 and 2, just like before.
    In this program, the value placed in address 1 is called the noun,
    and the value placed in address 2 is called the verb.
    Each of the two input values will be between 0 and 99, inclusive.
     */

    findNounAndVerbForSolution(getInts(inputLine), 19690720).get
  }

  def getInput: IO[String] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day2.txt"))
          .take(1)
      }
      .compile
      .toList
      .map(_.head)

  def getInts(inputLine: String): List[Int] =
    inputLine
      .split(",")
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
        .updated(2, verb)
    )

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

  def intCodeProgram(inputs: List[Int]): List[Int] = {

    @scala.annotation.tailrec
    def helper(currentInstruction: Int, ints: List[Int]): List[Int] = {
      ints.get(currentInstruction) match {
        case Some(x) if x == 1 || x == 2 =>
          val positionValue1 = ints(currentInstruction + 1)
          val positionValue2 = ints(currentInstruction + 2)
          val positionOutput = ints(currentInstruction + 3)

          val value1 = ints(positionValue1)
          val value2 = ints(positionValue2)

          val result =
            if (x == 1)
              value1 + value2
            else
              value1 * value2

          helper(currentInstruction + 4, ints.updated(positionOutput, result))

        case Some(99) =>
          //done
          ints

        case _ =>
          throw new RuntimeException("something went wrong")
      }
    }

    helper(0, inputs)
  }

  def intCodeProgram(inputLine: String): String =
    intCodeProgram(getInts(inputLine)).mkString(",")
}
