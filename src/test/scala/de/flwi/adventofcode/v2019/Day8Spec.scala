package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.Day8._
import org.scalatest.{FunSuite, Matchers}

class Day8Spec extends FunSuite with Matchers {

  val input = getInput.unsafeRunSync()

  test("Part 1 - example") {
    //converting layers to List[Int] to make them comparable, since Array(Array(1)) is not equals Array(Array(1))
    val layers = getLayers(3, 2, imageData("123456789012"))

    layers shouldBe List(
      List(
        List(1, 2, 3),
        List(4, 5, 6),
      ),
      List(
        List(7, 8, 9),
        List(0, 1, 2)
      )
    )
  }

  test("Part 1") {
    println(part1(input))
  }

  test("Part 2 - example - validate layer-parsing, just to be sure") {
    val layers = getLayers(2, 2, imageData("0222112222120000"))
    layers shouldBe List(
      List(
        List(0, 2),
        List(2, 2)
      ),
      List(
        List(1, 1),
        List(2, 2)
      ),
      List(
        List(2, 2),
        List(1, 2)
      ),
      List(
        List(0, 0),
        List(0, 0)
      )
    )
  }

  test("Part 2 - getPixelColor") {
    //layers stack from bottom to top
    //0 is black
    //1 is white
    //2 is transparent

    //The top-left pixel is black because the top layer is 0.
    getPixelColor(List(0, 1, 2, 0)) shouldBe 0

    //The top-right pixel is white because the top layer is 2 (transparent), but the second layer is 1.
    getPixelColor(List(2, 1, 2, 0)) shouldBe 1

    //The bottom-left pixel is white because the top two layers are 2, but the third layer is 1.
    getPixelColor(List(2, 2, 1, 0)) shouldBe 1

    //The bottom-right pixel is black because the only visible pixel in that position is 0 (from layer 4).  }
    getPixelColor(List(2, 2, 2, 0)) shouldBe 0
  }

  test("Part 2 example - pixelLists") {
    val layers: List[List[List[Int]]] = getLayers(2, 2, imageData("0222112222120000"))

    val pixelLists = getPixelLists(layers)
    pixelLists shouldBe List(
      List(
        List(0, 1, 2, 0),
        List(2, 1, 2, 0)
      ),
      List(
        List(2, 2, 1, 0),
        List(2, 2, 2, 0)
      )
    )
  }

  test("Part 2 example - pixelColors") {
    val layers     = getLayers(2, 2, imageData("0222112222120000"))
    val pixelLists = getPixelLists(layers)
    val pixelColors: List[List[Int]] = pixelLists.map(_.map(getPixelColor))

    pixelColors shouldBe List(
        List(0,1),
        List(1,0)
    )
  }

  test("Part 2") {
    println(part2(input))
  }

  test("Part 2 - own example with 3x2 input (4 layers)") {
    //first 3 layers transparent
    //last layer: top row white, bottom row black
    val layers = getLayers(3,2, imageData("222222222222222222111000"))
    val pixelLists = getPixelLists(layers)
    val pixelColors: List[List[Int]] = pixelLists.map(_.map(getPixelColor))

    layers.size shouldBe 4
    layers.map(_.size).distinct shouldBe List(2)
    layers.flatMap(_.map(_.size)).distinct shouldBe List(3)

    pixelLists.size shouldBe 2 //two rows
    pixelLists.map(_.size).distinct shouldBe List(3) //three pixels per row
    pixelLists.flatMap(_.map(_.size)).distinct shouldBe List(4) //four layers per pixel

    pixelColors.size shouldBe 2 //two rows
    pixelColors.map(_.size).distinct shouldBe List(3) //three pixels per row

    pixelColors shouldBe List(
      List(1,1,1),
      List(0,0,0)
    )
  }
}
