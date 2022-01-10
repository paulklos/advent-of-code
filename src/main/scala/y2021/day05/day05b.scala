package y2021.day05

import scala.io.Source

object day05b extends App {

  case class Line(start: (Int, Int), end: (Int, Int)) {

    def expand: List[(Int, Int)] = {
      if (start._1 == end._1)
        // Vertical
        if (start._2 <= end._2)
          (for (y <- start._2 to end._2) yield (start._1, y)).toList
        else
          (for (y <- end._2 to start._2) yield (start._1, y)).toList

      else if (start._2 == end._2)
        // Horizontal
        if (start._1 <= end._1)
          (for (x <- start._1 to end._1) yield (x, start._2)).toList
        else
          (for (x <- end._1 to start._1) yield (x, start._2)).toList

      else
        if (start._1 <= end._1) {
          val yStep = if (start._2 <= end._2) 1 else -1
          (for (x <- 0 to end._1 - start._1) yield (start._1 + x, start._2 + x * yStep)).toList
        } else {
          val yStep = if (start._2 >= end._2) 1 else -1
          (for (x <- 0 to end._1 - start._1 by -1) yield (start._1 + x, start._2 + x * yStep)).toList
        }
    }

  }

  def readInput: List[String] = {
    val file = getClass.getResource("input").getFile
    val input = Source.fromFile(file)

    val lines = input.getLines().toList

    input.close()

    lines
  }

  val linesInput = readInput

  def parsePointString(pointString: String): (Int, Int) = {
    val parts = pointString.split(",")
    if (parts.size == 2) {
      (parts(0).toInt, parts(1).toInt)
    } else {
      throw new IllegalArgumentException
    }
  }

  def countPoints(lines: List[Line]):Array[Array[Int]] = {

    val dimensions = day05a.determineMax(lines.flatMap(line => List(line.start, line.end)))

    println(f"Creating array of ${dimensions._1 + 1} by ${dimensions._2 + 1}")
    val points = Array.ofDim[Int](dimensions._1 + 1, dimensions._2 + 1)

    lines
      .flatMap(line => {
        println("Processing line " + line)
        line.expand
      })
      .foreach(point => {
        println("Adding point " + point)
        points(point._1)(point._2) = points(point._1)(point._2) + 1
      })

    points
  }

  def pointsWithCount(points: Array[Array[Int]]): Int = {
    points.foldLeft(0)((acc, row) => acc + row.count(count => count >=2))
  }

  def parseInput(input: List[String]): List[Line] = {
    input.map(s => {
      val pointStrings = s.split(" -> ")
      if (pointStrings.size == 2)
        Line(parsePointString(pointStrings(0)), parsePointString(pointStrings(1)))
      else
        throw new IllegalArgumentException
    })
  }

  val lines = parseInput(linesInput)

  val points = countPoints(lines)

  val pointsWithAtLeastTwoLines = pointsWithCount(points)

  println(pointsWithAtLeastTwoLines)
}
