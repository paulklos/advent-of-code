package y2021.day15

import y2021.day08.Day08a.readInput
import y2021.day09.Day09a
import y2021.day15.Day15a.lowestRiskRoute
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
