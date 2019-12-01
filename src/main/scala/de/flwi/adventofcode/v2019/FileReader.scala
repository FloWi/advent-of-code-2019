package de.flwi.adventofcode.v2019

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.{io, text, Stream}
import java.nio.file.{Path, Paths}
import cats.effect.ContextShift



object FileReader {
  def lines(blocker: Blocker, path: Path)(
      implicit contextShift: ContextShift[IO]
  ): Stream[IO, String] =
    io.file
      .readAll[IO](path, blocker, 4096)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(s => !s.isEmpty())

}
