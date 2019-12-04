package de.flwi.adventofcode.v2019

import java.nio.file.Paths

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream

/*
=================
== PART 1
=================

 */

object Day4 extends IOApp {

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

  def part1(input: String): Int = {
    /*
You arrive at the Venus fuel depot only to discover it's protected by a password.
The Elves had written the password on a sticky note, but someone threw it out.

However, they do remember a few key facts about the password:

It is a six-digit number.
The value is within the range given in your puzzle input.
Two adjacent digits are the same (like 22 in 122345).
Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
Other than the range rule, the following are true:

111111 meets these criteria (double 11, never decreases).
223450 does not meet these criteria (decreasing pair of digits 50).
123789 does not meet these criteria (no double).
How many different passwords within the range given in your puzzle input meet these criteria?

Your puzzle input is 387638-919123.
     */

    val from = input.split("-").head.toInt
    val to   = input.split("-").last.toInt

    def isValidPassword(pwd: Int) = is6DigitsLong(pwd) && containsTwoAdjacentDigitsThatAreTheSame(pwd) && onlyIncreasingIntegers(pwd)

    val validPasswords = from
      .to(to)
      .filter(isValidPassword)

    validPasswords.size
  }

  def is6DigitsLong(pwd: Int): Boolean =
    math.log10(pwd).toInt == 5
  def containsTwoAdjacentDigitsThatAreTheSame(pwd: Int): Boolean =
    pwd.toString
      .sliding(size = 2, step = 1)
      .filter(_.length == 2)
      .exists(str => str.distinct.length == 1)

  def containsTwoAdjacentDigitsThatAreTheSameButNotPartOfLargerGroup(pwd: Int): Boolean = {

    @scala.annotation.tailrec
    def helper(current: List[Char],
               currentIndex: Int,
               lastMatchingIndex: Option[Int],
               lastChar: Option[Char],
               result: List[(Char, Int, Int)]): List[(Char, Int, Int)] =
      //cases:
      // - starting (currentIndex == 0)        --> call with new lastMatchingIndex and lastChar
      // - same char (currentChar == lastChar) --> just call next step
      // - new char (currentChar != lastChar)  --> add entry to result
      // - end (current isEmpty)                --> return result
      current match {
        case Nil =>
          (lastChar.get, lastMatchingIndex.get, currentIndex) :: result

        case ::(head, tl) if currentIndex == 0 =>
          helper(tl, currentIndex + 1, lastMatchingIndex = Some(currentIndex), lastChar = Some(head), result)

        case ::(head, tl) =>
          if (lastChar.contains(head)) {
            helper(tl, currentIndex + 1, lastMatchingIndex, lastChar, result)
          }
          else {
            helper(
              tl,
              currentIndex + 1,
              lastMatchingIndex = Some(currentIndex),
              lastChar = Some(current.head),
              (lastChar.get, lastMatchingIndex.get, currentIndex) :: result
            )
          }
      }

    val result = helper(pwd.toString.toList, 0, None, None, List.empty).map { case (char, start, end) => (char, end - start) }

    result.exists(_._2 == 2)
  }

  def onlyIncreasingIntegers(pwd: Int): Boolean =
    pwd.toString
      .sliding(size = 2, step = 1)
      .filter(_.length == 2)
      .forall { str =>
        val first  = str(0).toInt
        val second = str(1).toInt

        first <= second
      }

  def part2(input: String): Int = {
    val from = input.split("-").head.toInt
    val to   = input.split("-").last.toInt

    def isValidPassword(pwd: Int) = is6DigitsLong(pwd) && containsTwoAdjacentDigitsThatAreTheSameButNotPartOfLargerGroup(pwd) && onlyIncreasingIntegers(pwd)

    val validPasswords = from
      .to(to)
      .filter(isValidPassword)

    validPasswords.size

  }
  def getInput: IO[String] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day4.txt")).head
      }
      .compile
      .lastOrError

}
