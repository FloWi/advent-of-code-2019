package de.flwi.adventofcode.v2019.day12

import cats.kernel.{Monoid, Semigroup}

case class Coord3D(x: Int, y: Int, z: Int) {
  val total: Int = math.abs(x)+math.abs(y)+math.abs(z)
}
case class Velocity(x: Int, y: Int, z: Int) {
  val total: Int = math.abs(x)+math.abs(y)+math.abs(z)
}

object Velocity {
  implicit val semigroup: Monoid[Velocity] = new Monoid[Velocity] {
    override def combine(v1: Velocity, v2: Velocity): Velocity = {
      Velocity(
        v1.x + v2.x,
        v1.y + v2.y,
        v1.z + v2.z
      )
    }

    override def empty: Velocity = Velocity(0,0,0)
  }


}
