package de.flwi.adventofcode.v2019.day11

import de.flwi.adventofcode.v2019.Location

object Model {

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

}
