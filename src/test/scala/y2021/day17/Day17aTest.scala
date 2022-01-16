package y2021.day17

import org.scalatest.{Assertions, GivenWhenThen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17aTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  trait ExampleArea {
      val area: Area = Area.parse("target area: x=20..30, y=-10..-5")
  }

  "An input string" should "be parsed correctly into an Area" in {
    Given("An input string")
    val input = "target area: x=20..30, y=-10..-5"

    When("The input is parsed")
    val area = Area.parse(input)

    Then("The Area is correct")
    area shouldEqual Area(Point(20, -5), Point(30, -10))
  }

  "A Point to the left of an Area" should "be outside the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    And("A point")
    val point = Point(5, -15)

    When("The point is checked")
    val result = area.isInside(point)

    Then("The Point is outside the Area")
    result shouldEqual false
  }

  "A Point to the left of an Area" should "not be over the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    And("A point")
    val point = Point(5, -15)

    When("The point is checked")
    val over = area.isOver(point)

    Then("The Point is not over the Area")
    over shouldEqual false
  }

  "A Point to the right of an Area" should "be outside the Area and over it" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    And("A point")
    val point = Point(25, -15)

    When("The point is checked")
    val result = area.isInside(point)

    Then("The Point is outside the Area")
    result shouldEqual false
  }

  "A Point to the right of an Area" should "be over it" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    And("A point")
    val point = Point(25, -15)

    When("The point is checked")
    val over = area.isOver(point)

    Then("The Point is over the Area")
    over shouldEqual true
  }

  "A Point above an Area" should "be outside the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    When("The point is checked")
    val result = area.isInside(Point(15, -5))

    Then("The Point is outside the Area")
    result shouldEqual false
  }

  "A Point above an Area" should "not be over it" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    When("The point is checked")
    val result = area.isOver(Point(15, -5))

    Then("The Point is outside the Area")
    result shouldEqual false
  }

  "A Point below an Area" should "be outside the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    When("The point is checked")
    val result = area.isInside(Point(15, -25))

    Then("The Point is outside the Area")
    result shouldEqual false
  }

  "A Point below an Area" should "be over the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    When("The point is checked")
    val result = area.isOver(Point(15, -25))

    Then("The Point is outside the Area")
    result shouldEqual true
  }

  "The top-left corner of an Area" should "be inside the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    When("The point is checked")
    val result = area.isInside(Point(10, -10))

    Then("The Point is inside the Area")
    result shouldEqual true
  }

  "The bottom-left corner of an Area" should "be inside the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    When("The point is checked")
    val result = area.isInside(Point(10, -20))

    Then("The Point is inside the Area")
    result shouldEqual true
  }

  "The top-right corner of an Area" should "be inside the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    When("The point is checked")
    val result = area.isInside(Point(20, -10))

    Then("The Point is inside the Area")
    result shouldEqual true
  }

  "The bottom-right corner of an Area" should "be inside the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    When("The point is checked")
    val result = area.isInside(Point(20, -20))

    Then("The Point is inside the Area")
    result shouldEqual true
  }

  "A point within the Area" should "be inside the Area" in {
    Given("An Area")
    val area = Area(Point(10, -10), Point(20, -20))

    When("The point is checked")
    val result = area.isInside(Point(15, -15))

    Then("The Point is inside the Area")
    result shouldEqual true
  }

  "The velocity for the next step" should "be decreased by one" in {
    Given("A velocity")
    val velocity = Velocity(10, 20)

    When("The velocity for the next step is calculated")
    val next = velocity.next

    Then("Both directions are decreased")
    next shouldEqual Velocity(9, 19)
  }

  "The velocity in horizontal direction" should "not go below zero" in {
    Given("A velocity")
    val velocity = Velocity(0, 0)

    When("The velocity for the next step is calculated")
    val next = velocity.next

    Then("Only the vertical direction is decreased")
    next shouldEqual Velocity(0, -1)
  }

  "The velocity in horizontal direction" should "not be below zero" in {
    Given("A velocity with a negative horizontal direction")
    val velocity = Velocity(-1, -1)

    Then("An exception is thrown when next is invoked")
    Assertions intercept[IllegalStateException] {
      velocity.next
    }
  }

  "The trajectory" should "calculate the next position" in {
    Given("A trajectory")
    val trajectory = Trajectory(List(Point(0, 0)), Velocity(1, 4))

    And("The next iteration is calculated")
    val next: Trajectory = trajectory.next

    Then("The next point is added to the route, the next velocity is calculated and the initial velocity remains the same")
    next shouldEqual Trajectory(List(Point(1, 4), Point(0,0)), Velocity(0,3), Velocity(1, 4))
  }

  "The example area" should "be reached in 7 steps" in new ExampleArea {
    Given("A velocity")
    private val velocity = Velocity(7,2)

    When("Seven steps are taken")
    private val end = (1 to 7).foldLeft(Trajectory(List(Point(0,0)), velocity))((t, _) => t.next)

    Then("The position should be in the target area")
    area.isInside(end.route.head) shouldEqual true
  }

  "The example area" should "have a highest trajectory with y = 45 when the starting velocity is (6, 9)" in new ExampleArea {
    When("The trajectory is evaluated")
    private val result = area.findHighestTrajectoryTo

    Then("A trajectory should be found")
    result.isEmpty shouldEqual false

    And("Its height should be 45")
    private val trajectory: Trajectory = result.get
    println("Found trajectory: " + trajectory)
    trajectory.highest.y shouldEqual 45
    trajectory.route(trajectory.route.size - 2) shouldEqual Point(6, 9)
  }

}
