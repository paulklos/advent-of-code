package y2021.day09

import y2021.day08.Day08a.readInput
import y2021.day09.Day09a.{determineLowPoints, parseLines, sumOfLowPoints}
import y2021.day09.Day09b.{findBasins, findLowPoints}
import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class Day09bTest {

  @Test
  def theLowPointsAreDetermined(): Unit = {
    val lines = readInput(getClass.getResource("testInput").getFile)

    val heights = parseLines(lines)
    val points = determineLowPoints(heights)

    val result = findLowPoints(points)

    assertEquals(4, result.size)

    assertTrue(result.contains(1, 0))
    assertTrue(result.contains(9, 0))
    assertTrue(result.contains(2, 2))
    assertTrue(result.contains(6, 4))
  }

  @Test
  def example(): Unit = {
    val lines = readInput(getClass.getResource("testInput").getFile)

    val heights = parseLines(lines)
    val points = determineLowPoints(heights)
    val lowPoints = findLowPoints(points)
    val result = findBasins(heights, lowPoints)

    assertEquals(4, result.size)

    val sizes = result.map(basin => basin.size).sorted
    assertEquals(List(3, 9, 9, 14), sizes)
  }


}
