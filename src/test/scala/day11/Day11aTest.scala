package day11

import org.junit.Assert.assertEquals
import org.junit.Test

class Day11aTest {

  @Test
  def oneStep(): Unit = {
    val lines = List(
      "11111",
      "19991",
      "19191",
      "19991",
      "11111")
    val dumboOctopuses = DumboOctopuses.create(lines)

    val flashes = dumboOctopuses.step

    assertEquals(9, flashes)
    assertEquals("3454340004500054000434543", dumboOctopuses.toString)
  }

  @Test
  def twoSteps(): Unit = {
    val lines = List(
      "11111",
      "19991",
      "19191",
      "19991",
      "11111")
    val dumboOctopuses = DumboOctopuses.create(lines)

    val flashes = dumboOctopuses.doSteps(2)

    assertEquals(9, flashes)
    assertEquals("4565451115611165111545654", dumboOctopuses.toString)
  }

}
