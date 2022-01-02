package day13

import day08.Day08a.readInput
import day13.Day13a.{parseCoordinates, parseFoldInstructions}
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13aTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "A list of lines" should "be parsed into a list of coordinates" in {
    Given("input lines")
    val lines = List(
      "1,2",
      "1,7",
      "",
      "fold along y=3")

    When( "The input is parsed")
    val result = parseCoordinates(lines)

    Then("The size should be 2")
    result.size shouldEqual 2

    And("should contain the correct elements")
    result should contain allOf ((1, 2), (1,7))
  }

  "A list of lines" should "be parsed into a list of fold instructions" in {
    Given("input lines")
    val lines = List(
      "1,2",
      "1,7",
      "",
      "fold along y=3",
      "fold along x=5")

    When( "The input is parsed")
    val result = parseFoldInstructions(lines)

    Then("The size should be 2")
    result.size shouldEqual 2

    And("should contain the correct elements")
    result should contain allOf (HFold(3), VFold(5))
  }

  "A Grid" should "be created from list of points" in {
    Given("a list of points")
    val points = List((1,2), (1,7), (11, 3))

    When( "A grid is created")
    val grid = Grid.create(points)

    Then("The size is the maximum of the ows and the columns")
    grid shouldEqual Grid(12, 8)

    And("The three points from the input are set")
    grid.isVisible(2, 1) shouldBe true
    grid.isVisible(7, 1) shouldBe true
    grid.isVisible(3, 11) shouldBe true
  }

  "A Grid" should "return the number of visible dots" in {
    Given("a list of points")
    val points = List((1,2), (1,7), (11, 3), (5, 4))

    When( "A grid is created")
    val grid = Grid.create(points)

    Then("The size is the maximum of the ows and the columns")
    grid shouldEqual Grid(12, 8)

    And("There are 4 visible dots")
    grid.visibleDots shouldBe 4
  }

  "A empty grid" should "remain empty when folded horizontally" in {
    Given("A grid")
    val grid = Grid(1, 3)

    And("a folding instruction to fold it in half")
    val fold = HFold(1)

    When("The grid is folded")
    val result = grid.fold(fold)

    Then("The result has a height of 1")
    result.height shouldEqual 1

    And("a width of 1")
    result.width shouldEqual 1
  }

  "A grid value that is set" should "remain set when the grid is folded horizontally" in {
    Given("A grid")
    val grid = Grid(1, 3)
    grid.loadValues(List((0, 0)))

    And("a folding instruction to fold it in half")
    val fold = HFold(1)

    When("The grid is folded")
    val result = grid.fold(fold)

    Then("The top-left field is set")
    result.isVisible(0, 0) shouldEqual true
  }

  "A grid value that is not set" should "is set when combined with a set value when the grid is folded horizontally" in {
    Given("A grid")
    val grid = Grid(1, 3)
    grid.loadValues(List((0, 2)))

    And("a folding instruction to fold it in half")
    val fold = HFold(1)

    When("The grid is folded")
    val result = grid.fold(fold)

    Then("The top-left field is set")
    result.isVisible(0, 0) shouldEqual true
  }

  "A grid value that is set" should "remains set when combined with a set value when the grid is folded horizontally" in {
    Given("A grid")
    val grid = Grid(1, 3)
    grid.loadValues(List((0, 2), (0, 0)))

    And("a folding instruction to fold it in half")
    val fold = HFold(1)

    When("The grid is folded")
    val result = grid.fold(fold)

    Then("The top-left field is set")
    result.isVisible(0, 0) shouldEqual true
  }

  "The example grid" should " have 17 visible dots after the first fold" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput1").getFile)
    val coordinates = parseCoordinates(lines)
    val foldInstructions = parseFoldInstructions(lines)

    And("a grid created from that")
    val grid = Grid.create(coordinates)

    And("The first fold")
    val fold = foldInstructions.head

    When("The grid is folded")
    val result = grid.fold(fold)

    Then("There should be 17 visible dots")
    result.visibleDots shouldEqual 17
  }

  "The example grid" should " have 16 visible dots after all the folds" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput1").getFile)
    val coordinates = parseCoordinates(lines)
    val foldInstructions = parseFoldInstructions(lines)

    And("a grid created from that")
    val grid = Grid.create(coordinates)

    When("The grid is folded")
    val result = grid.fold(foldInstructions)

    Then("There should be 17 visible dots")
    result.visibleDots shouldEqual 16
  }

}
