package day07

import day07.Day07b.determineAscendingCostPerPosition
import org.junit.Assert.assertEquals
import org.junit.Test

class Day07bTest {

  @Test
  def example(): Unit = {
    val positions = List(16,1,2,0,4,2,7,1,2,14)

    val result = determineAscendingCostPerPosition(positions)

    println(result)
    assertEquals(206L, result(2))
    assertEquals(168L, result(5))

    assertEquals(result.minBy(_._2), (5, 168L))
  }

}
