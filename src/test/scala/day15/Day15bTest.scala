package day15

import day08.Day08a.readInput
import day09.Day09a
import day15.Day15b.lowestRiskRoute
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15bTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "A risk grid" should "be multiplied correctly" in {
    Given("A grid")
    val risks = List(List(4, 1), List(7, 8))

    When("The grid is multiplied")
    val result = Day15b.multiplyGrid(risks)

    And("should contain the correct elements")
    result should contain allOf (
      List(4,1,5,2,6,3,7,4,8,5),
      List(7,8,8,9,9,1,1,2,2,3),
      List(5,2,6,3,7,4,8,5,9,6),
      List(8,9,9,1,1,2,2,3,3,4),
      List(6,3,7,4,8,5,9,6,1,7),
      List(9,1,1,2,2,3,3,4,4,5),
      List(7,4,8,5,9,6,1,7,2,8),
      List(1,2,2,3,3,4,4,5,5,6),
      List(8,5,9,6,1,7,2,8,3,9),
      List(2,3,3,4,4,5,5,6,6,7))

  }

  "The multiplied example grid" should "have a least risk of 315" in {
    Given("An input file")
    val lines = readInput(getClass.getResource("testInput").getFile)

    And("which is parsed into a map of risks")
    val risks = Day09a.parseLines(lines)

    And("The grid is multiplied")
    val multi = Day15b.multiplyGrid(risks)
    //Day15b.printGrid(multi)

    When("The route with the lowest risk is determined")
    val result = lowestRiskRoute(multi)

    Then("The result has a risk of 315")
    result.risk shouldEqual 315
  }
}
