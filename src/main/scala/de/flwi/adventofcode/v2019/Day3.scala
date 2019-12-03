package de.flwi.adventofcode.v2019

import java.nio.file.Paths

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream

/*
=================
== PART 1
=================

 */

object Day3 extends IOApp {
  def manhattanDistance(a: Point, b: Point): Int =
    math.abs(a.x - b.x) + math.abs(a.y - b.y)

  def getAllPointsOfPath(path: List[Point]): List[Point] = {
    val fullPath = path
      .sliding(2, 1)
      .flatMap {
        case List(_) =>
          //reached end
          Nil
        case List(start, end) =>
          val diffX = end.x - start.x
          val diffY = end.y - start.y
          val xs    = 0.to(diffX, if (diffX > 0) 1 else -1)
          val ys    = 0.to(diffY, if (diffY > 0) 1 else -1)

          val newPoints = xs.flatMap(x => ys.map(y => Point(start.x + x, start.y + y)))
          //the head of newpoints would contain a node that has already been visited, so we only need the tail
          newPoints.tail
      }
      .toList

    path.headOption.toList ++ fullPath
  }

  def intersections(firstWire: String, secondWire: String): IntersectionResult = {

    val firstPathPoints  = getPathPoints(Point(0, 0), getMoves(firstWire))
    val secondPathPoints = getPathPoints(Point(0, 0), getMoves(secondWire))

    val firstFullPathPoints  = getAllPointsOfPath(firstPathPoints) //.toSet
    val secondFullPathPoints = getAllPointsOfPath(secondPathPoints)
    val intersectionPoints = firstFullPathPoints
      .intersect(secondFullPathPoints)
      .diff(List(Point(0, 0)))

    IntersectionResult(firstFullPathPoints, secondFullPathPoints, intersectionPoints)

  }

  def getPathPoints(start: Point, moves: List[Move]): List[Point] =
    moves
      .scanLeft(start)((prev, move) => getEndpoint(prev, move))

  sealed trait Move

  case class Up(value: Int)    extends Move
  case class Down(value: Int)  extends Move
  case class Left(value: Int)  extends Move
  case class Right(value: Int) extends Move

  case class IntersectionResult(firstFullPathPoints: List[Point], secondFullPathPoints: List[Point], intersectionPoints: List[Point])

  def getMoves(directions: String): List[Move] =
    directions
      .split(",")
      .toList
      .map { moveStr =>
        val num = moveStr.tail.toInt
        moveStr.head match {
          case 'U' => Up(num)
          case 'D' => Down(num)
          case 'L' => Left(num)
          case 'R' => Right(num)
          case _   => throw new IllegalArgumentException
        }
      }

  def getEndpoint(start: Point, move: Move): Point =
    move match {
      case Up(value) =>
        start.copy(y = start.y + value)

      case Down(value) =>
        val result = start.copy(y = start.y - value)
        result

      case Left(value) =>
        start.copy(x = start.x - value)

      case Right(value) =>
        start.copy(x = start.x + value)
    }

  case class Point(x: Int, y: Int)

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

  def part1(input: List[String]): Int = {
    val List(firstWire, secondWire) = input
    val result: IntersectionResult =
      Day3.intersections(firstWire, secondWire)

    result.intersectionPoints.map(Day3.manhattanDistance(Point(0, 0), _)).min
  }

  def part2(input: List[String]): Int = {
    val List(firstWire, secondWire) = input
    val result: IntersectionResult =
      Day3.intersections(firstWire, secondWire)

    val numberOfSteps = result.intersectionPoints.map{ intersectionPoint =>
      val positionInFirstWire = result.firstFullPathPoints.indexOf(intersectionPoint)
      val positionInSecondWire = result.secondFullPathPoints.indexOf(intersectionPoint)
      (intersectionPoint, positionInFirstWire, positionInSecondWire, positionInFirstWire+positionInSecondWire)
    }

    val minPoint = numberOfSteps.minBy(_._4)
    minPoint._4
  }

  def getInput: IO[List[String]] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day3.txt"))
      }
      .compile
      .toList

}
