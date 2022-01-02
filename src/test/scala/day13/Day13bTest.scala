package day13

import day08.Day08a.readInput
import day13.Day13a.{parseCoordinates, parseFoldInstructions}
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13bTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "The grid" should " have 17 visible dots after the first fold" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput2").getFile)
    val coordinates = parseCoordinates(lines)
    val foldInstructions = parseFoldInstructions(lines)

    And("a grid created from that")
    val grid = Grid.create(coordinates)

    When("The grid is folded")
    val result = grid.fold(foldInstructions)

    Then("There should be 5 visible dots")
    result.visibleDots shouldEqual 5

    And("The dots are correctly placed")
    result.isVisible(2, 0) shouldBe true
    result.isVisible(1, 1) shouldBe true
    result.isVisible(1, 2) shouldBe true
    result.isVisible(2, 0) shouldBe true
    result.isVisible(0, 2) shouldBe true
  }

}
