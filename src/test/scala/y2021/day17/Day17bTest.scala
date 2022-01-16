package y2021.day17

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17bTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  trait ExampleArea {
      val area: Area = Area.parse("target area: x=20..30, y=-10..-5")
  }

  "The minimum y velocity to match the given x velocity" should "be calculated correctly" in {
    Given("A target area")
    val area = Area(Point(27, -16), Point(40, -23))

    And("An x velocity of 7")
    val vx = 7

    When("A starting velocity is calculated")
    val v = Velocity.min(vx, area)

    Then("The vertical velocity is -18")
    v shouldEqual Velocity(vx, -18)
  }

  "The example area" should "have 112 different initial velocities that end up inside the target area" in new ExampleArea {
    When("The possible velocities are evaluated")
    private val result = area.possibleVelocities()

    Then("Multiple should be found")
    result.isEmpty shouldEqual false

    And("The number should be 112")
    result.size shouldEqual 112
  }

}
