package y2021.day05

import y2021.day05.day05a.{Line, countPoints, parseInput, pointsWithCount}
import org.junit.Assert.assertEquals
import org.junit.Test

class day05aTest {

  @Test
  def aLineOfOnePoint():Unit = {
    val line = Line((0,0), (0,0))

    assertEquals(List((0,0)), line.expand)
  }

  @Test
  def aVerticalLine():Unit = {
    val line = Line((0,0), (0,2))

    assertEquals(List((0,0), (0,1), (0,2)), line.expand)
  }

  @Test
  def anInvertedVerticalLine():Unit = {
    val line = Line((0,2), (0,0))

    assertEquals(List((0,0), (0,1), (0,2)), line.expand)
  }

  @Test
  def aHorizontalLine():Unit = {
    val line = Line((0,0), (2,0))

    assertEquals(List((0,0), (1,0), (2,0)), line.expand)
  }

  @Test
  def anInvertedHorizontalLine():Unit = {
    val line = Line((2,0), (0,0))

    assertEquals(List((0,0), (1,0), (2,0)), line.expand)
  }

  @Test
  def anotherHorizontalLine():Unit = {
    val line = Line((3, 4), (1, 4))

    assertEquals(List((1,4), (2,4), (3,4)), line.expand)
  }

  @Test
  def pointCountIsCorrect(): Unit = {
    val line1 = Line((2,0), (0,0))
    val line2 = Line((0,0), (0,2))

    val points = countPoints(List(line1, line2))

    assertEquals(2, points(0)(0))
    assertEquals(1, points(0)(1))
    assertEquals(1, points(0)(2))
    assertEquals(1, points(1)(0))
    assertEquals(1, points(2)(0))
    assertEquals(0, points(1)(1))
    assertEquals(0, points(1)(2))
    assertEquals(0, points(2)(1))
    assertEquals(0, points(2)(2))
  }

  @Test
  def pointsWithCountAtLeastTwoIsCorrect(): Unit = {
    val input = List(
      "0,9 -> 5,9",
      "8,0 -> 0,8",
      "9,4 -> 3,4",
      "2,2 -> 2,1",
      "7,0 -> 7,4",
      "6,4 -> 2,0",
      "0,9 -> 2,9",
      "3,4 -> 1,4",
      "0,0 -> 8,8",
      "5,5 -> 8,2")

    val lines = parseInput(input)

    val points = countPoints(lines)

    assertEquals(5, pointsWithCount(points))
  }

}
