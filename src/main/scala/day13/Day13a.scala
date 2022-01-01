package day13

import day05.day05a.determineMax
import day08.Day08a.readInput

import scala.annotation.tailrec

abstract class FoldLine {
  val value: Int
}

case class HFold(value: Int) extends FoldLine

case class VFold(value: Int) extends FoldLine

case class Grid(width: Int, height: Int) {
  private val grid: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)

  def loadValues(points: List[(Int, Int)]): Unit = {
    points.foreach {
      case (col, row) => grid(row)(col) = true
    }
  }

  def isSet(row: Int, col: Int): Boolean = {
    grid(row)(col)
  }

  def fold(foldLine: FoldLine): Grid = {
    foldLine match {
      case HFold(y) =>
        val newGrid = Grid(width, y + 1)

        for (row <- 0 until y; col <- 0 until width)
          newGrid.grid(row)(col) = grid(row)(col) || grid(2 * y - row)(col)

        newGrid
      case VFold(x) =>
        val newGrid = Grid(x + 1, height)

        for (row <- 0 until height; col <- 0 until x)
          newGrid.grid(row)(col) = grid(row)(col) || grid(row)(2 * x - col)

        newGrid
    }
  }

  def fold(foldInstructions: List[FoldLine]): Grid = {
    foldInstructions.foldLeft(this)((grid, foldLine) => grid.fold(foldLine))
  }

  def visibleDots: Long = {
    grid.foldLeft(0L)((acc, row) => acc + row.count(dot => dot))
  }

  def printIt(): Unit = {
    grid.foreach(row => {
      print(row.map(b => if (b) "#" else ".").mkString)
      println
    })
    println
  }

}

object Grid {

  def create(points: List[(Int, Int)]): Grid = {
    val max = determineMax(points)
    val grid = max match {
      case (maxRow, maxCol) => Grid(maxRow + 1, maxCol + 1)
    }

    grid.loadValues(points)
    grid.printIt()

    grid
  }
}

object Day13a extends App {

  def parseCoordinates(lines: List[String]): List[(Int, Int)] = {

    val linePattern = "(\\d*),(\\d*)".r

    @tailrec
    def iter(acc: List[(Int, Int)], toParse: List[String]): List[(Int, Int)] = {


      toParse match {
        case Nil => acc
        case x::xs =>
          x match {
            case "" => acc
            case linePattern(x, y) => iter((x.toInt, y.toInt) :: acc, xs)
          }
      }
    }

    iter(List(), lines)
  }

  def parseFolInstructions(lines: List[String]): List[FoldLine] = {
    val linePattern = "fold along (x|y)=(\\d*)".r

    @tailrec
    def iter(acc: List[FoldLine], toParse: List[String]): List[FoldLine] = {

      toParse match {
        case Nil => acc
        case x::xs =>
          x match {
            case linePattern(x, y) => x match {
              case "x" => iter(acc appended VFold(y.toInt), xs)
              case "y" => iter(acc appended HFold(y.toInt), xs)
            }
            case _ => iter(acc, xs)
          }
      }
    }

    iter(List(), lines)
  }

  val lines = readInput(getClass.getResource("input").getFile)
  val coordinates = parseCoordinates(lines)
  val foldInstructions = parseFolInstructions(lines)

  val grid = Grid.create(coordinates)

  val result = grid.fold(foldInstructions.head)

  result.printIt()
  println("Visible lines after the first fold: " + result.visibleDots)

}
