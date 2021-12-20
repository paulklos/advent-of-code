package day07

import day07.Day07a.{determineFixedCostPerPosition, determineRange}
import org.junit.Assert.assertEquals
import org.junit.Test

class Day07aTest {

  @Test
  def theZeroZeroRangeIsDeterminedCorrectly(): Unit = {
    val values = List(0, 0)

    val range = determineRange(values)

    assertEquals((0, 0), range)
  }

  @Test
  def theOneOneRangeIsDeterminedCorrectly(): Unit = {
    val values = List(1, 1)

    val range = determineRange(values)

    assertEquals((1, 1), range)
  }

  @Test
  def aRangeIsDeterminedCorrectly(): Unit = {
    val values = List(1, 7)

    val range = determineRange(values)

    assertEquals((1, 7), range)
  }

  @Test
  def aReversedRangeIsDeterminedCorrectly(): Unit = {
    val values = List(1000, 362)

    val range = determineRange(values)

    assertEquals((362, 1000), range)
  }

  @Test
  def example(): Unit = {
    val positions = List(16,1,2,0,4,2,7,1,2,14)

    val result = determineFixedCostPerPosition(positions)

    println(result)
    assertEquals(41L, result(1))
    assertEquals(37L, result(2))
    assertEquals(39L, result(3))
    assertEquals(71L, result(10))

    assertEquals(result.minBy(_._2), (2, 37))
  }

}
