package day09

import day08.Day08a.readInput

object Day09a extends App {

  case class PointData(x: Int, y:Int, low: Boolean, risk: Int = 0)

  def parseLine(line: String): List[Int] = {
    line.foldLeft(List[Int]())((acc, c) => acc appended c.asDigit)
  }

  // Create a two-dimensional list of heights
  def parseLines(lines: List[String]): List[List[Int]] = {
    lines.foldLeft(List[List[Int]]())((acc, line) => acc appended parseLine(line))
  }


  def determineLowPoints(heights: List[List[Int]]): List[List[PointData]] = {
    // This gives us a row of values, plus the current column value y
    for ((row, y) <- heights.zipWithIndex) yield {
      // This gives the height of the cell, plus the row value x
      for ((height, x) <- row.zipWithIndex) yield {

        // See if the current point is a low in horizontal direction, i.e. check its neighbors over x
        val horizontalLow = (for (yi <- x - 1 to x + 1 by 2 if yi >= 0 && yi < row.size) yield heights(y)(yi) > height)
          .foldLeft(true)((acc, value) => value && acc)
        // Same for the vertical direction
        val verticalLow = (for (xi <- y - 1 to y + 1 by 2 if xi >= 0 && xi < heights.size) yield heights(xi)(x) > height)
          .foldLeft(true)((acc, value) => value && acc)

        if (verticalLow && horizontalLow) PointData(x, y, low = true, height + 1) else PointData(x, y, low = false)
      }
    }
  }

  // Filter the points that are lows, and add their values together
  def sumOfLowPoints(points: List[List[PointData]]): Int = {
    points.foldLeft(0)((acc, row) => acc + row.filter(pointData => pointData.low).map(pointData => pointData.risk).sum)
  }

  private val lines = readInput(getClass.getResource("input").getFile)

  private val heights = parseLines(lines)

  private val points = determineLowPoints(heights)

  println("Sum of low point risk levels: " + sumOfLowPoints(points))

}
