package day12

import day08.Day08a.readInput
import day12.Day12a.{orderPaths, pathMap, printPath}
import day12.Day12b.getAllPaths
import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class Day12bTest {

  @Test
  def exampleCave1(): Unit = {
    val lines = readInput(getClass.getResource("testInput1").getFile)
    val routes = pathMap(lines)

    val result = orderPaths(getAllPaths(routes)).map(l => printPath(l))

    assertEquals(36, result.size)
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
    assertTrue(result.contains("start,A,b,d,b,A,c,A,end"))
  }

  @Test
  def exampleCave2(): Unit = {
    val lines = readInput(getClass.getResource("testInput2").getFile)
    val routes = pathMap(lines)

    val result = getAllPaths(routes).map(l => printPath(l))

    assertEquals(103, result.size)
  }

  @Test
  def exampleCave3(): Unit = {
    val lines = readInput(getClass.getResource("testInput3").getFile)
    val routes = pathMap(lines)

    val result = getAllPaths(routes).map(l => printPath(l))

    assertEquals(3509, result.size)
  }

}
