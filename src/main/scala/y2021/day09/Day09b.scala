package y2021.day09

import y2021.day08.Day08a.readInput
import y2021.day09.Day09a.{PointData, determineLowPoints, parseLines}

import scala.annotation.tailrec

object Day09b extends App {

  def findLowPoints(points: List[List[PointData]]): List[(Int, Int)] = {
    // For each row, filter the low points, map them to their coordinates, and collect the lows into a single list
    points.foldLeft(List[(Int, Int)]())((acc, row) => acc ::: row.filter(p => p.low).map(p => (p.x, p.y)))
  }

  def findBasins(heights: List[List[Int]], lowPoints: List[(Int, Int)]): List[Set[(Int, Int)]] = {

    /*
     * Recursively add a point to the current basin.
     *
     * The point is added if it qualifies. If so, the function is called for its four neighbors.
     */
    def addPoint(basin: Set[(Int, Int)], refHeight: Int, point: (Int, Int)): Set[(Int, Int)] = {
      println("Checking point " + point)
      if (point._1 >= heights.head.size || point._2 >= heights.size || point._1 < 0 || point._2 < 0) {
        // point outside the grid -> done
        basin
      } else {
        val height = heights(point._2)(point._1)
        if (height == 9 || height <= refHeight) {
          // Point not part of the basin -> done
          basin
        } else {
          println("Adding point")
          val newBasin = basin + point
          // Potentially add the neighboring points
          val newPoints = List((point._1 + 1, point._2), (point._1 - 1, point._2), (point._1, point._2 + 1), (point._1, point._2 - 1))
          newPoints.foldLeft(newBasin)((acc, point) => addPoint(acc, height, point))
        }
      }
    }

    @tailrec
    def createBasin(acc: List[Set[(Int, Int)]], lowsToCheck: List[(Int, Int)]): List[Set[(Int, Int)]] = {
      lowsToCheck match {
        case Nil => acc
        case point::xs =>
          println(f"Creating basin for low point $point")
          // Start with refHeight zero, so the low point always gets added
          val basin = addPoint(Set(), 0, point)
          println("Adding basin " + basin)
          createBasin(basin :: acc, xs)
      }
    }

    createBasin(List(), lowPoints)
  }

  private val lines = readInput(getClass.getResource("input").getFile)

  private val heights = parseLines(lines)

  private val points = determineLowPoints(heights)

  private val lowPoints = findLowPoints(points)

  val basins = findBasins(heights, lowPoints)

  println("Found basins: " + basins)

  private val basinSizes: List[Int] = basins.map(basin => basin.size).sorted.reverse

  println("Basin sizes: " + basinSizes)

  println("Basin sizes product: " + (basinSizes take 3).product)

}
