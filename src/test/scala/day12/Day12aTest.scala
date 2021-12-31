package day12

import day08.Day08a.readInput
import day12.Day12a.{pathMap, getAllPaths, printPath, removeCave}
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import org.scalatest.Assertions

class Day12aTest {

  @Test
  def aCaveWithALowerCaseNameIsSmall(): Unit = {
    val cave = Cave("some cave name")

    assertTrue(cave.isSmall)
  }

  @Test
  def aCaveWithAUpperCaseCharacterIsNotSmall(): Unit = {
    val cave = Cave("some cave Nme")

    assertFalse(cave.isSmall)
  }

  @Test
  def aLineIsAPathBetweenTwoCaves(): Unit = {
    val line = "start-end"

    val path = Path.parseLine(line)

    assertEquals(Path(Cave("start"), Cave("end")), path)
  }

  @Test
  def parsingAFaultyLineThrowsAnException(): Unit = {
    val line = "start_end"

    Assertions intercept[IllegalArgumentException] {
      Path.parseLine(line)
    }
  }

  @Test
  def aPathHasTwoDirections(): Unit = {
    val line = "a-B"

    val destinations = pathMap(Path.parseLine(line))

    assertEquals(destinations(Cave("a")), Cave("B"))
    assertEquals(destinations(Cave("B")), Cave("a"))
  }

  @Test
  def pathsAreTranslatedToDirections(): Unit = {
    val lines = List("a-B", "a-c")

    val destinations = pathMap(lines)

    assertEquals(List(Cave("B"), Cave("c")), destinations(Cave("a")))
    assertEquals(List(Cave("a")), destinations(Cave("B")))
    assertEquals(List(Cave("a")), destinations(Cave("c")))
  }

  @Test
  def aCaveCanBeRemovedFromTheRoutes(): Unit = {
    val caveA = Cave("a")
    val caveB = Cave("b")
    val caveC = Cave("c")
    val routes = Map(caveA -> List(caveB, caveC), caveB -> List(caveC), caveC -> List(caveB))

    val updated = removeCave(routes, caveB)

    assertFalse(updated.contains(caveB))
    assertFalse(updated.contains(caveC))
    assertEquals(List(caveC), updated(caveA))
  }

  @Test
  def exampleCave1(): Unit = {
    val lines = readInput(getClass.getResource("testInput1").getFile)
    val routes = pathMap(lines)

    val result = getAllPaths(routes).map(l => printPath(l))

    assertEquals(10, result.size)
    assertTrue(result.contains("start,A,b,A,c,A,end"))
    assertTrue(result.contains("start,A,b,A,end"))
    assertTrue(result.contains("start,A,b,end"))
    assertTrue(result.contains("start,A,c,A,b,A,end"))
    assertTrue(result.contains("start,A,c,A,b,end"))
    assertTrue(result.contains("start,A,c,A,end"))
    assertTrue(result.contains("start,A,end"))
    assertTrue(result.contains("start,b,A,c,A,end"))
    assertTrue(result.contains("start,b,A,end"))
    assertTrue(result.contains("start,b,end"))
  }

  @Test
  def exampleCave2(): Unit = {
    val lines = readInput(getClass.getResource("testInput2").getFile)
    val routes = pathMap(lines)

    val result = getAllPaths(routes).map(l => printPath(l))

    assertEquals(19, result.size)
  }

  @Test
  def exampleCave3(): Unit = {
    val lines = readInput(getClass.getResource("testInput3").getFile)
    val routes = pathMap(lines)

    val result = getAllPaths(routes).map(l => printPath(l))

    assertEquals(226, result.size)
  }

}
