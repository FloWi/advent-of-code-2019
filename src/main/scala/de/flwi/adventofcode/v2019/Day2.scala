package de.flwi.adventofcode.v2019

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.{io, text, Stream}
import java.nio.file.{Path, Paths}

/*
=================
== PART 1
=================


 */

object Day2 extends IOApp {

  import FileReader._

  def solver =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day2.txt"))
          .showLines(Console.out)
      }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- solver.compile.drain
    } yield ExitCode.Success
}
