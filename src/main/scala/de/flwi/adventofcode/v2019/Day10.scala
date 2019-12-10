package de.flwi.adventofcode.v2019

import java.nio.file.{Path, Paths}

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import de.flwi.adventofcode.v2019.IntCodeComputer.getInts
import fs2.Stream

import scala.reflect.internal.util.Origins.OriginId

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

  def part1(inputLine: String): String = {
    val initial = IntCodeComputer(getInts(inputLine), instructionPointer = 0, inputValues = Vector(1L), outputValues = Vector.empty, relativeBase = 0, id = "")
    val actual  = initial.run()

    actual.outputValues.toString()
  }

  def part2(inputLine: String): String = {
    val initial = IntCodeComputer(getInts(inputLine), instructionPointer = 0, inputValues = Vector(2L), outputValues = Vector.empty, relativeBase = 0, id = "")
    val actual  = initial.run()

    actual.outputValues.toString()
  }

  def getInput: IO[String] =
    getInput(Paths.get("data/day10.txt"))

  def getInput(filePath: Path): IO[String] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, filePath)
          .take(1)
      }
      .compile
      .toList
      .map(_.head)

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

  def findBestAsteroidForMonitoringStation(input: String): Location = {
    val asteroidMap = Location.parseMap(input)

    //for-each asteroid
    //  select every other asteroid that is visible

    val visiblesByCandidate = asteroidMap.asteroidLocations.map { candidate =>
      (candidate, findVisible(asteroidMap, candidate))
    }

    val result = visiblesByCandidate.maxBy(_._2.size)
    result._1
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
    val offset     = end - start
    val normalized = offset.normalized

    val mapSize = mapHeight + mapHeight
    val result = (mapSize * -1)
      .to(mapSize)
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

    Location(x / gcd_, y / gcd_)
  }

  def distance: Int = math.abs(x) + math.abs(y)
}
