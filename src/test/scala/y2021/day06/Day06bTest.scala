package y2021.day06

import y2021.day06.Day06b.{addDay, addDays, toMap}
import org.junit.Assert.assertEquals
import org.junit.Test

class Day06bTest {

  @Test
  def thePopulationIsCalculatedCorrectly(): Unit = {
    val lanternFish = List(3,4,3,1,2)

    val result = toMap(Map(), lanternFish)
    assertEquals(1L, result(1))
    assertEquals(1L, result(2))
    assertEquals(2L, result(3))
    assertEquals(1L, result(4))
  }

  @Test
  def aLanternFishThatWillSpawnInSixDaysWillSpawnInFiveAfterADay(): Unit = {
    val lanternFish = toMap(Map(), List(6))

    val result = addDay(lanternFish)
    assertEquals(1L, result(5))
    assertEquals(Option.empty, result.get(6))
  }

  @Test
  def aLanternFishThatWillSpawnInFiveDaysWillSpawnInFourAfterADay(): Unit = {
    val lanternFish = toMap(Map(), List(5))

    val result = addDay(lanternFish)
    assertEquals(1L, result(4))
    assertEquals(Option.empty, result.get(5))
  }

  @Test
  def aLanternFishThatWillSpawnInFourDaysWillSpawnInThreeAfterADay(): Unit = {
    val lanternFish = toMap(Map(), List(4))

    val result = addDay(lanternFish)
    assertEquals(1L, result(3))
    assertEquals(Option.empty, result.get(4))
  }

  @Test
  def aLanternFishThatWillSpawnInThreeDaysWillSpawnInTwoAfterADay(): Unit = {
    val lanternFish = toMap(Map(), List(3))

    val result = addDay(lanternFish)
    assertEquals(1L, result(2))
    assertEquals(Option.empty, result.get(3))
  }

  @Test
  def aLanternFishThatWillSpawnInTwoDaysWillSpawnInOneAfterADay(): Unit = {
    val lanternFish = toMap(Map(), List(2))

    val result = addDay(lanternFish)
    assertEquals(1L, result(1))
    assertEquals(Option.empty, result.get(2))
  }

  @Test
  def aLanternFishThatWillSpawnInOneDaysWillSpawnInZeroAfterADay(): Unit = {
    val lanternFish = toMap(Map(), List(1))

    val result = addDay(lanternFish)
    assertEquals(1L, result(0))
    assertEquals(Option.empty, result.get(1))
  }

  @Test
  def aLanternFishThatWillSpawnInZeroDaysSpawnsAndWillSpawnAgainInSixDays(): Unit = {
    val lanternFish = toMap(Map(), List(0))

    val result = addDay(lanternFish)
    assertEquals(1L, result(6))
    assertEquals(1L, result(8))
    assertEquals(Option.empty, result.get(0))
  }

  @Test
  def spawning(): Unit = {
    val lanternFish = toMap(Map(), List(0, 1))

    val result = addDay(lanternFish)
    assertEquals(1L, result(6))
    assertEquals(1L, result(8))
    assertEquals(1L, result(0))
  }

  @Test
  def example(): Unit = {
    val lanternFish = toMap(Map(), List(3,4,3,1,2))
    val day1 = addDay(lanternFish)

    assertEquals(1L, day1(0))
    assertEquals(1L, day1(1))
    assertEquals(2L, day1(2))
    assertEquals(1L, day1(3))

    val day2a = addDay(day1)
    assertEquals(1L, day2a(6))
    assertEquals(1L, day2a(8))
    assertEquals(1L, day2a(0))
    assertEquals(2L, day2a(1))
    assertEquals(1L, day2a(2))

    val day7 = addDays(7, lanternFish)
    assertEquals(1L, day7(1))
    assertEquals(1L, day7(2))
    assertEquals(3L, day7(3))
    assertEquals(2L, day7(4))
    assertEquals(2L, day7(5))
    assertEquals(1L, day7(6))
  }

  @Test
  def addingTwoDays(): Unit = {
    val lanternFish = toMap(Map(), List(3,4,3,1,2))

    val result = addDays(2, lanternFish)
    assertEquals(1L, result(6))
    assertEquals(1L, result(8))
    assertEquals(1L, result(0))
    assertEquals(2L, result(1))
    assertEquals(1L, result(2))
  }

  @Test
  def addingThreeDays(): Unit = {
    val lanternFish = toMap(Map(), List(3,4,3,1,2))

    val result = addDays(3, lanternFish)
    assertEquals(2L, result(0))
    assertEquals(1L, result(1))
    assertEquals(1L, result(5))
    assertEquals(1L, result(6))
    assertEquals(1L, result(7))
    assertEquals(1L, result(8))
  }

  @Test
  def addingFourDays(): Unit = {
    val lanternFish = toMap(Map(), List(3,4,3,1,2))

    val result = addDays(4, lanternFish)
    assertEquals(1L, result(0))
    assertEquals(1L, result(4))
    assertEquals(1L, result(5))
    assertEquals(3L, result(6))
    assertEquals(1L, result(7))
    assertEquals(2L, result(8))
  }

  @Test
  def addingEighteenDays(): Unit = {
    val lanternFish = toMap(Map(), List(3,4,3,1,2))

    val result = addDays(18, lanternFish)
    assertEquals(3L, result(0))
    assertEquals(5L, result(1))
    assertEquals(3L, result(2))
    assertEquals(2L, result(3))
    assertEquals(2L, result(4))
    assertEquals(1L, result(5))
    assertEquals(5L, result(6))
    assertEquals(1L, result(7))
    assertEquals(4L, result(8))

    assertEquals(26L, result.values.sum)
  }

  @Test
  def adding256Days(): Unit = {
    val lanternFish = toMap(Map(), List(3,4,3,1,2))

    val result = addDays(256, lanternFish)

    assertEquals(26984457539L, result.values.sum)
  }

}
