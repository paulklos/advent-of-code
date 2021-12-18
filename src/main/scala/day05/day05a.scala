package day05

import scala.annotation.tailrec
import scala.io.Source

object day05a extends App {

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
        List()
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

    @tailrec
    def determineMax(acc: (Int, Int), lines: List[(Int, Int)]): (Int, Int) = {

      def getMax(current: (Int, Int)): (Int, Int) = {
        val maxX = if (current._1 > acc._1) current._1 else acc._1
        val maxY = if (current._2 > acc._2) current._2 else acc._2
        (maxX, maxY)
      }

      lines match {
        case x::Nil => getMax(x)
        case x::xs => determineMax(getMax(x), xs)
      }
    }

    val dimensions = determineMax((0, 0), lines.flatMap(line => List(line.start, line.end)))

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
