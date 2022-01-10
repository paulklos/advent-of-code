package y2021.day06

import y2021.day06.Day06a.{addDay, addDays}
import org.junit.Assert.assertEquals
import org.junit.Test

class Day06aTest {

  @Test
  def aLanternFishThatWillSpawnInSixDaysWillSpawnInFiveAfterADay(): Unit = {
    val lanternFish = List(6)

    assertEquals(List(5), addDay(lanternFish))
  }

  @Test
  def aLanternFishThatWillSpawnInFiveDaysWillSpawnInFourAfterADay(): Unit = {
    val lanternFish = List(5)

    assertEquals(List(4), addDay(lanternFish))
  }

  @Test
  def aLanternFishThatWillSpawnInFourDaysWillSpawnInThreeAfterADay(): Unit = {
    val lanternFish = List(4)

    assertEquals(List(3), addDay(lanternFish))
  }

  @Test
  def aLanternFishThatWillSpawnInThreeDaysWillSpawnInTwoAfterADay(): Unit = {
    val lanternFish = List(3)

    assertEquals(List(2), addDay(lanternFish))
  }

  @Test
  def aLanternFishThatWillSpawnInTwoDaysWillSpawnInOneAfterADay(): Unit = {
    val lanternFish = List(2)

    assertEquals(List(1), addDay(lanternFish))
  }

  @Test
  def aLanternFishThatWillSpawnInOneDaysWillSpawnInZeroAfterADay(): Unit = {
    val lanternFish = List(1)

    assertEquals(List(0), addDay(lanternFish))
  }

  @Test
  def aLanternFishThatWillSpawnInZeroDaysSpawnsAndWillSpawnAgainInSixDays(): Unit = {
    val lanternFish = List(0)

    assertEquals(List(6, 8), addDay(lanternFish))
  }

  @Test
  def spawning(): Unit = {
    val lanternFish = List(0, 1)

    assertEquals(List(6, 0, 8), addDay(lanternFish))
  }

  @Test
  def example(): Unit = {
    val lanternFish = List(3,4,3,1,2)
    val day1 = addDay(lanternFish)

    assertEquals(List(2,3,2,0,1), day1)

    val day2 = addDay(day1)
    assertEquals(List(1,2,1,6,0,8), day2)

    val day7 = addDays(7, lanternFish)
    assertEquals(List(3,4,3,1,2,3,4,5,5,6), day7)

    val day18 = addDays(18, lanternFish)
    assertEquals(List(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8), day18)

    assertEquals(26, day18.count(_ => true))

  }
}
