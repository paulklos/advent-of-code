package y2021.day09

import y2021.day08.Day08a.readInput
import y2021.day09.Day09a.{PointData, determineLowPoints, parseLine, parseLines, sumOfLowPoints}
import org.junit.Assert.assertEquals
import org.junit.Test

class Day09aTest {

  @Test
  def aLineIsParsedToListOfHeights(): Unit = {
    val line = "7678999878"

    val result = parseLine(line)

    assertEquals(List(7, 6, 7, 8, 9, 9, 9, 8, 7, 8), result)
  }

  @Test
  def aFileIsParsedToATwoDimensionalListOfHeights(): Unit = {
    val lines = readInput(getClass.getResource("testInput").getFile)

    val result = parseLines(lines)

    assertEquals(5, result.size)

    assertEquals(List(2,1,9,9,9,4,3,2,1,0), result.head)
    assertEquals(List(3,9,8,7,8,9,4,9,2,1), result(1))
    assertEquals(List(9,8,5,6,7,8,9,8,9,2), result(2))
    assertEquals(List(8,7,6,7,8,9,6,7,8,9), result(3))
    assertEquals(List(9,8,9,9,9,6,5,6,7,8), result(4))
  }

  @Test
  def theLowPointsAreDeterminedCorrectly(): Unit = {
    val line1 = List(1, 3)
    val line2 = List(4, 2)
    val lines = List(line1, line2)

    val result = determineLowPoints(lines)

    assertEquals(PointData(0, 0, low = true, 2), result.head.head)
    assertEquals(PointData(1, 0, low = false), result.head(1))
    assertEquals(PointData(0, 1, low = false), result(1).head)
    assertEquals(PointData(1, 1, low = true, 3), result(1)(1))
  }

  @Test
  def example(): Unit = {
    val lines = readInput(getClass.getResource("testInput").getFile)

    val result = sumOfLowPoints(determineLowPoints(parseLines(lines)))

    assertEquals(15, result)
  }

}
