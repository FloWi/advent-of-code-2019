package de.flwi.adventofcode.v2019.day12

import java.nio.file.{Path, Paths}

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.kernel.Monoid
import de.flwi.adventofcode.v2019.FileReader.lines
import fs2.{Pure, Stream}

object Day12 extends IOApp {

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
    val moons = input.map(parseCoord)

    val result: (Map[Int, Coord3D], Map[Int, Velocity]) = simulate(moons).take(1000).last.toList.head.get

    ???
  }

  def potentialEnergy(coord3D: Coord3D): Int = {
    val Coord3D(x, y, z) = coord3D
    x + y + z
  }

  def kineticEnergy(velocity: Velocity): Int = {
    val Velocity(x, y, z) = velocity
    x + y + z
  }

  def applyGravityCalcDeltaV(moonPair: List[(Int, Coord3D)]): List[(Int, Velocity)] = {
    val (i1, Coord3D(x1, y1, z1)) :: (i2, Coord3D(x2, y2, z2)) :: Nil = moonPair

    val vx1 = if (x1 > x2) -1 else if (x1 < x2) 1 else 0
    val vy1 = if (y1 > y2) -1 else if (y1 < y2) 1 else 0
    val vz1 = if (z1 > z2) -1 else if (z1 < z2) 1 else 0

    List(
      (i1, Velocity(vx1, vy1, vz1)),
      (i2, Velocity(-vx1, -vy1, -vz1)),
    )
  }

  def simulate(moons: List[Coord3D]): Stream[Pure, (Map[Int, Coord3D], Map[Int, Velocity])] = {
    import cats.implicits._

    val initial = (
      moons.zipWithIndex.map(_.swap).toMap,
      List.fill(moons.size)(Velocity(0, 0, 0)).zipWithIndex.map(_.swap).toMap
    )

    Stream
      .constant((), 1)
      .zipWithIndex
      .map(_._2)
      .scan(initial) {
        case ((indexedMoons, indexedVelocities), step) =>
          //update the velocity of each moon by applying gravity

          val gravityResult = indexedMoons.toList
            .combinations(2)
            .toList
            .flatMap(applyGravityCalcDeltaV)

          val dVs = gravityResult
            .groupBy(_._1)
            .mapValues(_.map(_._2))
            .mapValues(Monoid[Velocity].combineAll(_))

          val updatedVelocities = dVs.combine(indexedVelocities)

          val moonsAfterGravity = updatedVelocities.foldLeft(indexedMoons) {
            case (indexedMoons, (idx, dV)) =>
              val Coord3D(x, y, z)     = indexedMoons(idx)
              val Velocity(dx, dy, dz) = dV
              val res = indexedMoons.updated(idx, Coord3D(x + dx, y + dy, z + dz))
              res
          }

          (moonsAfterGravity, updatedVelocities)
      }

  }

  def part2(input: List[String]): String =
    ???

  def parseCoord(line: String): Coord3D = {
    val regex = raw"""<\w=(-?\d+),\s+\w=(-?\d+),\s+\w=(-?\d+)>""".r
    //<x=-10, y=-13, z=7>
    line match {
      case regex(x, y, z) => Coord3D(x.toInt, y.toInt, z.toInt)
    }
  }

  def getInput: IO[List[String]] =
    getInput(Paths.get("data/day12.txt"))

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
  //<x=-10, y=-13, z=7>

}
