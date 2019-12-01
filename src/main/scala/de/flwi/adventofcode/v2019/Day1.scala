package de.flwi.adventofcode.v2019

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.{io, text, Stream}
import java.nio.file.{Path, Paths}

/*

=================
== PART 1
=================
Fuel required to launch a given module is based on its mass. Specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2.

For example:

For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
For a mass of 1969, the fuel required is 654.
For a mass of 100756, the fuel required is 33583.
The Fuel Counter-Upper needs to know the total fuel requirement. To find it, individually calculate the fuel needed for the mass of each module (your puzzle input), then add together all the fuel values.

=================
== PART 2
=================

Fuel itself requires fuel just like a module -
take its mass, divide by three, round down, and subtract 2.
However, that fuel also requires fuel, and that fuel requires fuel, and so on. Any mass that would require negative fuel should instead be treated as if it requires zero fuel; the remaining mass, if any, is instead handled by wishing really hard, which has no mass and is outside the scope of this calculation.

So, for each module mass, calculate its fuel and add it to the total.
Then, treat the fuel amount you just calculated as the input mass and
repeat the process, continuing until a fuel requirement is zero or negative.
 */

object Day1 extends IOApp {

  import FileReader._

  def solver(fuelEquation: Int => Int) =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day1.txt"))
          .map(_.toInt)
          .map(fuelEquation)
          .fold(0)(_ + _)
          .showLines(Console.out)
      }

  def getFuel(mass: Int): Int = mass / 3 - 2

  def getFuelRecursively(mass: Int): Int = {
    def helper(mass: Int, acc: Int): Int = {
      val fuelMass = getFuel(mass)
      if (fuelMass <= 0) acc
      else helper(fuelMass, acc + fuelMass)
    }

    helper(mass, 0)
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- solver(getFuel).compile.drain
      _ <- solver(getFuelRecursively).compile.drain
    } yield ExitCode.Success
}
