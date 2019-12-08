package de.flwi.adventofcode.v2019

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Paths

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream
import javax.imageio.ImageIO

object Day8 extends IOApp {

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
    //layers not in pixel rows, but all pixels of layer in one list
    val layers: List[List[Int]] = getLayers(25, 6, imageData(inputLine)).map(_.toList.flatMap(_.toList))

    val layerWithMinAmountOfZeroes = layers.minBy(_.count(_ == 0))
    val numberOfOnes               = layerWithMinAmountOfZeroes.count(_ == 1)
    val numberOfTwos               = layerWithMinAmountOfZeroes.count(_ == 2)

    s"number of 1 digits * number of 2 digits of layer with fewest 0 digits = ${numberOfOnes * numberOfTwos}"
  }

  def part2(inputLine: String): String = {
    val layers     = getLayers(25, 6, imageData(inputLine))
    val pixelLists = getPixelLists(layers)
    val pixels     = pixelLists.map(_.map(getPixelColor))

    val file = new File("result-day8-part2.png")
    ImageIO.write(renderImage(pixels), "png", file)

    s"rendered image to ${file.getAbsolutePath}"
  }

  def getInput: IO[String] =
    Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        lines(blocker, Paths.get("data/day8.txt"))
          .take(1)
      }
      .compile
      .toList
      .map(_.head)

  def imageData(input: String): Array[Int] =
    input.toCharArray.map(_.toString.toInt)

  def getLayers(width: Int, height: Int, imageData: Array[Int]): List[List[List[Int]]] = {
    val pixelRows = imageData.sliding(width, width).toList
    val layers    = pixelRows.sliding(height, height).toList

    layers
      .map(_.toArray)
      .map(_.toList.map(_.toList))
  }

  def getPixelLists(layers: List[List[List[Int]]]): List[List[List[Int]]] =
    // example: 2x2 image with 4 layers
    // layers:
    // List[4] --> 4 layers
    //   List[2] --> 2 rows per layer
    //     List[2]  --> 2 pixels per row

    // result should be
    // List[2] two rows
    //   List[2] two pixels per row
    //    List[4] four layers per pixel
    layers.transpose(l => l.flatten).grouped(layers.head.head.size).toList

  def getPixelColor(pixelsOfLayers: List[Int]): Int =
    //find first pixel with non-transparent color
    pixelsOfLayers.find(_ != 2).get

  def renderImage(pixels: List[List[Int]]): BufferedImage = {
    val image = new BufferedImage(25, 6, BufferedImage.TYPE_BYTE_GRAY)
    pixels.zipWithIndex.foreach {
      case (row, y) =>
        row.zipWithIndex.foreach {
          case (p, x) =>
            val color = if (p == 0) Color.BLACK else Color.WHITE
            image.setRGB(x, y, color.getRGB)
        }
    }
    image
  }
}
