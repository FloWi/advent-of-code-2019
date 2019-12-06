package de.flwi.adventofcode.v2019

import java.nio.file.Paths

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream

object Day6 extends IOApp {

  case class Planet(id: String, center: Option[String] = None)

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

  def parsePlanetMap(inputs: List[String]): Map[String, Planet] = {
    val map = collection.mutable.HashMap.empty[String, Planet]

    inputs.foreach { line =>
      val List(centerId, orbitingBodyId) = line.split(')').toList

      map.getOrElseUpdate(centerId, Planet(centerId, None))

      val outerPlanet = map.getOrElseUpdate(orbitingBodyId, Planet(orbitingBodyId, Some(centerId)))
      map.update(orbitingBodyId, outerPlanet.copy(center = Some(centerId)))
    }
    map.toMap
  }

  def getNumberOfOrbitsOfPlanet(planetMap: Map[String, Planet], planet: Planet): Int = {

    @scala.annotation.tailrec
    def helper(currentPlanet: Planet, sum: Int): Int =
      currentPlanet.center match {
        case Some(centerId) => helper(planetMap(centerId), sum + 1)
        case None           => sum
      }
    helper(planet, 0)
  }

  def getOrbitChain(planetMap: Map[String, Planet], planetId: String): List[String] = {

    @scala.annotation.tailrec
    def helper(current: Planet, result: List[String]): List[String] =
      current.center match {
        case None =>
          current.id :: result

        case Some(centerId) =>
          helper(planetMap(centerId), current.id :: result)
      }
    helper(planetMap(planetId), List.empty)
  }

  def getTotalNumberOfOribts(planetMap: Map[String, Planet]): Int =
    planetMap.values.map(p => getNumberOfOrbitsOfPlanet(planetMap, p)).sum

  def findCommonCenter(pm: Map[String, Planet], id1: String, id2: String): Option[String] = {
    val chain1 = getOrbitChain(pm, id1)
    val chain2 = getOrbitChain(pm, id2)

    val chain1Set = chain1.toSet
    val chain2Set = chain2.toSet

    chain1.reverse.find(id => chain1Set.contains(id) && chain2Set.contains(id))
  }

  def getPlanetHopChain(pm: Map[String, Planet], id1: String, id2: String): List[String] = {
    val commonCenter = findCommonCenter(pm, id1, id2).get

    val chainId1 = getOrbitChain(pm, id1)
    val chainId2 = getOrbitChain(pm, id2)

    val id1ToCommonCenter = chainId1.reverse.tail.takeWhile(_ != commonCenter)
    val commonCenterToId2 = chainId2.dropWhile(_ != commonCenter).filterNot(_ == "SAN")

    (id1ToCommonCenter ++ commonCenterToId2).sliding(2, 1).map { case List(from, to) => s"$from to $to" }.toList

  }

  def part1(input: List[String]): String = {
    val planetMap      = parsePlanetMap(input)
    val numberOfOrbits = getTotalNumberOfOribts(planetMap)
    s"total number of direct and indirect orbits: ${numberOfOrbits}"
  }

  def part2(input: List[String]): String = {

    val pm = parsePlanetMap(input)

    val planetHopChain = getPlanetHopChain(pm, "YOU", "SAN")

    s"${planetHopChain.mkString("\n")}\nit takes ${planetHopChain.size} orbital transfers"
  }

  def getInput: IO[List[String]] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day6.txt"))
          .filter(_.nonEmpty)
      }
      .compile
      .toList

  val isDebug = true
  def myDebug(x: Any): Unit =
    if (isDebug) { Console.println(x) }

}
