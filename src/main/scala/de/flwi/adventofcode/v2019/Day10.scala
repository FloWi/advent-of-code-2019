package de.flwi.adventofcode.v2019

import java.nio.file.{Path, Paths}

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import de.flwi.adventofcode.v2019.IntCodeComputer.getInts
import fs2.Stream

object Day10 extends IOApp {

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

  def part1(input: String): String = {
    val solution = Location.findBestAsteroidForMonitoringStation(input)

    s"best location is ${solution._1} with ${solution._2.size} asteroids in sight"
  }

  def part2(inputLine: String): String =
    ???

  def getInput: IO[String] =
    getInput(Paths.get("data/day10.txt"))

  def getInput(filePath: Path): IO[String] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, filePath)
      }
      .compile
      .toList
      .map(_.mkString("\n"))

  val isDebug = false
  def myDebug(x: Any): Unit =
    if (isDebug) {
      Console.println(x)
    }
}

object Location {

  def findVisible(asteroidMap: AsteroidMap, candidate: Location): Set[Location] = {
    val result = asteroidMap.asteroidLocations.-(candidate).flatMap { other =>
      val b: Boolean = canSee(candidate, other, asteroidMap)
      if (b) List(other) else List.empty
    }
    result
  }

  def findBestAsteroidForMonitoringStation(input: String): (Location, Set[Location]) = {
    val asteroidMap = Location.parseMap(input)

    //for-each asteroid
    //  select every other asteroid that is visible

    val visiblesByCandidate = asteroidMap.asteroidLocations.map { candidate =>
      (candidate, findVisible(asteroidMap, candidate))
    }

    val result = visiblesByCandidate.maxBy(_._2.size)
    result
  }

  def canSee(origin: Location, other: Location, asteroidMap: AsteroidMap): Boolean = {
    val lineOfSight            = Location.lineOfSight(origin, other, asteroidMap.width, asteroidMap.height)
    val asteroidsInLineOfSight = asteroidMap.asteroidLocations.intersect(lineOfSight.toSet)

    val distance           = (other - origin).distance
    val isAnotherOneCloser = asteroidsInLineOfSight.exists(a => (a - origin).distance < distance)

    //is visible only if the other is the closest
    !isAnotherOneCloser
  }

  case class AsteroidMap(asteroidLocations: Set[Location], width: Int, height: Int)

  def parseMap(input: String): AsteroidMap = {

    val charMap: Array[Array[Char]] = input.split("\n").map(_.toCharArray)

    val asteroidLocations = for {
      y <- charMap.indices
      x <- charMap(y).indices
      char = charMap(y)(x)
      if char != '.'
    } yield Location(x, y)

    val height = charMap.length
    val width  = charMap.head.length
    AsteroidMap(asteroidLocations.toSet, width, height)
  }

  def fromTuple: ((Int, Int)) => Location = { case (x, y) => Location(x, y) }

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def lineOfSight(start: Location, end: Location, mapWidth: Int, mapHeight: Int): List[Location] = {
    val offset     = start - end
    val normalized = offset.normalized

    val mapSize = mapHeight + mapHeight

    val result =
      1.to(mapSize)
        .map { i =>
          start + Location(i * normalized.x, i * normalized.y)
        }
        .filter { case Location(x, y) => x >= 0 && x < mapWidth && y >= 0 && y < mapHeight }
        .toList

    result.diff(List(start, end))
  }
}

case class Location(x: Int, y: Int) {
  def -(other: Location): Location =
    Location(other.x - this.x, other.y - this.y)

  def +(other: Location): Location =
    Location(other.x + this.x, other.y + this.y)

  def normalized: Location = {
    val gcd_ = Location.gcd(x, y)
    val correctionFactorX =
      if (x < 0) -1
      else 1

    val correctionFactorY =
      if (y < 0) -1
      else 1

    Location(x / math.abs(gcd_), y / math.abs(gcd_))
  }

  def distance: Int = math.abs(x) + math.abs(y)
}
