package y2021.day11

import y2021.day08.Day08a.readInput
import org.junit.Assert.assertEquals
import org.junit.Test

class Day11bTest {

  @Test
  def theFirstSimultaneousFlashIsAfter195Steps(): Unit = {
    val lines = readInput(getClass.getResource("testInput").getFile)

    val dumboOctopuses = DumboOctopuses.create(lines)

    val turn = dumboOctopuses.firstSimultaneous

    assertEquals(195, turn)
  }

}
