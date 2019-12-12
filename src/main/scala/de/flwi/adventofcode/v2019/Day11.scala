package de.flwi.adventofcode.v2019

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.{Path, Paths}

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import fs2.Stream
import javax.imageio.ImageIO

object Day11 extends IOApp {

  import FileReader._

  def run(args: List[String]): IO[ExitCode] =
    for {
      input       <- getInput
      resultPart1 <- part1(input.head)
      _           <- IO(println("result part 1"))
      _           <- IO(println(resultPart1))
      resultPart2 <- part2(input.head)
      _           <- IO(println("result part 2"))
      _           <- IO(println(resultPart2))
    } yield ExitCode.Success

  def part1(inputLine: String): IO[String] = {
    val intCodeComputer = IntCodeComputer.intCodeProgramWithInput(inputLine, "")

    val result = solve(0, intCodeComputer, Location(0, 0), Direction.Up, Map(Location(0, 0) -> Color.Black))

    val image: BufferedImage = renderImage(result)

    val file = new File("result-day11-part1.png")
    IO(ImageIO.write(image, "png", file)) *>
      IO.pure(
        s"""
           |rendered image to ${file.getAbsolutePath}
           |the robot painted ${result.size} panels at least once\"\"\"
      |""".stripMargin.trim
      )
  }

  def part2(inputLine: String): IO[String] = {
    val intCodeComputer = IntCodeComputer.intCodeProgramWithInput(inputLine, "")

    val result = solve(0, intCodeComputer, Location(0, 0), Direction.Up, Map(Location(0, 0) -> Color.White))

    val image: BufferedImage = renderImage(result)

    val file = new File("result-day11-part2.png")
    IO(ImageIO.write(image, "png", file)) *>
      IO.pure(
        s"""rendered image to ${file.getAbsolutePath}
          |the robot painted ${result.size} panels at least once
          |""".stripMargin.trim
      )
  }

  def getInput: IO[List[String]] =
    getInput(Paths.get("data/day11.txt"))

  def getInput(filePath: Path): IO[List[String]] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, filePath)
      }
      .compile
      .toList

  /*
  The Intcode program will serve as the brain of the robot.

  Input for robot:
  The program uses input instructions to access the robot's camera:
  provide
    - 0 if the robot is over a black panel or
    - 1 if the robot is over a white panel.

  First, it will output a value indicating the color to paint the panel the robot is over:
    - 0 means to paint the panel black, and
    - 1 means to paint the panel white.

  Second, it will output a value indicating the direction the robot should turn:
    - 0 means it should turn left 90 degrees, and
    - 1 means it should turn right 90 degrees.

  After the robot turns, it should always move forward exactly one panel.

  The robot starts facing up.
   */

  def moveForward(loc: Location, currentDirection: Direction): Location =
    currentDirection match {
      case Direction.Up    => loc.copy(y = loc.y - 1)
      case Direction.Down  => loc.copy(y = loc.y + 1)
      case Direction.Left  => loc.copy(x = loc.x - 1)
      case Direction.Right => loc.copy(x = loc.x + 1)
    }

  def performTurn(currentDirection: Direction, turn: Turn): Direction =
    turn match {
      case Turn.Left =>
        currentDirection match {
          case Direction.Up    => Direction.Left
          case Direction.Right => Direction.Up
          case Direction.Down  => Direction.Right
          case Direction.Left  => Direction.Down
        }
      case Turn.Right =>
        currentDirection match {
          case Direction.Up    => Direction.Right
          case Direction.Right => Direction.Down
          case Direction.Down  => Direction.Left
          case Direction.Left  => Direction.Up
        }
    }

  def parseTurnDirection(int: Int): Turn =
    int match {
      case 0 => Turn.Left
      case 1 => Turn.Right
    }

  def parseColor(int: Int): Color =
    int match {
      case 0 => Color.Black
      case 1 => Color.White
    }

  def renderImage(result: Map[Location, Color]): BufferedImage = {

    val minX = result.keys.map(_.x).min
    val maxX = result.keys.map(_.x).max
    val minY = result.keys.map(_.y).min
    val maxY = result.keys.map(_.y).max
    val pixels = minY.to(maxY).toList.map { y =>
      minX.to(maxX).toList.map { x =>
        val color    = result.getOrElse(Location(x, y), Color.Black)
        val colorInt = if (color == Color.Black) 0 else 1
        colorInt
      }
    }

    val image = Day8.renderImage(pixels)

    image
  }

  @scala.annotation.tailrec
  def solve(idx: Int,
            intCodeComputer: IntCodeComputer,
            currentLocation: Location,
            currentDirection: Direction,
            panelColors: Map[Location, Color]): Map[Location, Color] = {

    val currentPanelColor = panelColors.getOrElse(currentLocation, Color.Black)
    val panelColorInputForRobot = currentPanelColor match {
      case Color.Black => 0
      case Color.White => 1
    }

    val newState = intCodeComputer.run(Vector(panelColorInputForRobot))

    if (newState.opCode == 99) {
      panelColors
    }
    else {
      val output = newState.outputValues.reverse
      if (output.size < 2) {
        if (idx % 100 == 0) {
          myDebug(s"idx: $idx; not enough input - recursing")
        }

        solve(idx + 1, newState, currentLocation, currentDirection, panelColors)
      }
      else {
        val Vector(col, dir) = output
        val turnDirection    = parseTurnDirection(dir.toInt)
        val color            = parseColor(col.toInt)

        myDebug(s"(${currentLocation.x}, ${currentLocation.y}) painting result $col - direction_result: $dir")

        val nextDirection = performTurn(currentDirection, turnDirection)
        val nextLocation  = moveForward(currentLocation, nextDirection)
        if (idx % 100 == 0) {
          myDebug(s"""
                                  |idx               : $idx
                                  |paintingResult    : $col
                                  |directionResult   : $dir
                                  |currentDirection: : $currentDirection
                                  |currentLocation   : $currentLocation
                                  |currentPanelColor : $currentPanelColor
                                  |output            : $output
                                  |turnDirection     : $turnDirection
                                  |color             : $color
                                  |nextDirection     : $nextDirection
                                  |nextLocation      : $nextLocation
                                  |""".stripMargin.trim)
        }
        solve(idx + 1, newState.removedOutput, nextLocation, nextDirection, panelColors.updated(currentLocation, color))
      }
    }
  }

  val isDebug = false
  def myDebug(x: Any): Unit =
    if (isDebug) {
      Console.println(x)
    }

  sealed trait Direction
  object Direction {

    case object Up extends Direction

    case object Down extends Direction

    case object Left extends Direction

    case object Right extends Direction

  }

  sealed trait Color
  object Color {
    case object Black extends Color
    case object White extends Color
  }

  sealed trait Turn
  object Turn {

    case object Left extends Turn

    case object Right extends Turn

  }

}
