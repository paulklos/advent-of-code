package day15

import day08.Day08a.readInput
import day09.Day09a
import day15.Day15a.lowestRiskRoute
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15aTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "The example grid" should "have a least risk of 40" in {
    Given("An input file")
    val lines = readInput(getClass.getResource("testInput").getFile)

    And("which is parsed into a map of risks")
    val risks = Day09a.parseLines(lines)

    When("The route with the lowest risk is determined")
    val result = lowestRiskRoute(risks)

    Then("The result has a risk of 40")
    result.risk shouldEqual 40
  }
}
