package y2021.day13

import y2021.day05.day05a.determineMax
import y2021.day08.Day08a.readInput

import scala.annotation.tailrec

/**
 * Indicates a fold at a certain position.
 */
abstract class FoldLine {
  val value: Int
}

/**
 * Horizontal fold.
 *
 * @param value the row to fold over
 */
case class HFold(value: Int) extends FoldLine

/**
 * Vertical fold.
 *
 * @param value the column to fold over
 */
case class VFold(value: Int) extends FoldLine

/**
 * This represents the grid of dots.
 *
 * @param width  the width, i.e the number of columns
 * @param height the height, i.e the number of rows
 */
case class Grid(width: Int, height: Int) {
  /**
   * The dot values, true means visible.
   *
   * Note that this array is mutable.
   */
  private val grid: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)

  /**
   * Set the visible points.
   *
   * @param points the visible points
   */
  def loadValues(points: List[(Int, Int)]): Unit = {
    points.foreach {
      case (col, row) => grid(row)(col) = true
    }
  }

  /**
   * Check if a dot is visible.
   *
   * @param row the row
   * @param col the column
   * @return true if the dot at the given position is visible.
   */
  def isVisible(row: Int, col: Int): Boolean = {
    grid(row)(col)
  }

  /**
   * Fold the grid over the indicated line.
   *
   * The new gid has a size (width or height, depending on whether it's a horizontal or a vertical fold) of the fold line. This means
   * the fold line itself is discarded, and not a part of the resulting grid.
   *
   * Imagine this grid, where the <code>#</code> indicate visible dots:
   * <pre>
   * ..#..
   * .#..#
   * #....
   * .....
   * #.#..
   * </pre>
   *
   * In case of a horizontal fold, i.e. a fold up over row 3, the dimensions of the new gid are (5, 3): the resulting grid will
   * still have 5 columns, but only three rows. For each row in the new grid, the value is the original value OR-ed together with
   * the value of the row that is folded onto it. So row 2 is combined with row 4, mirrored over the fold row. Or, as a formula
   *
   * <pre>
   * (r, c)' = (r, c) OR (f - r + f, c) = (r, c) OR (2f - r, c)
   * </pre>
   *
   * in which r is the row, c is the column and f indicates the fold row. The part <code>f - r + f</code> is the distance from the
   * current row to the fold row, added to the fold row to get to the mirror row of r.
   *
   * In case of row zero, this yields 8, which is outside the grid, because the fold is below the middle, so there's no row that lands on top
   * of the top row. In such a case, the original value remains unchanged.
   *
   * For a vertical fold, the formula becomes:
   *
   * <pre>
   * (r, c)' = (r, c) OR (r, f - c + f) = (r, c) OR (r, 2f - c)
   * </pre>
   *
   * After the horizontal fold over row 3, we end up with:
   *
   * <pre>
   * ..#..
   * .#..#
   * #.#..
   * </pre>
   *
   * The value (2, 2) is added from row 4, i.e. it was originally (2, 4).
   *
   * Finally, executing the vertical fold over column 3 yields:
   *
   * <pre>
   * ..#.
   * .##.
   * #.#.
   * </pre>
   *
   * @param foldLine the line to fold over
   * @return a new grid of smaller dimensions
   */
  def fold(foldLine: FoldLine): Grid = {
    println("Folding " + foldLine)

    val result = foldLine match {
      case HFold(y) =>
        // Create a new grid
        val newGrid = Grid(width, y)

        for (row <- 0 until y;
             col <- 0 until width) {
          // The mirrored row, or false if that's outside the grid
          val mirroredDotVisible = if (2 * y - row < height) grid(2 * y - row)(col) else false
          newGrid.grid(row)(col) = grid(row)(col) || mirroredDotVisible
        }

        newGrid
      case VFold(x) =>
        // Create a new grid
        val newGrid = Grid(x, height)

        for (row <- 0 until height;
             col <- 0 until x) {
          // The mirrored row, or false if that's outside the grid
          val mirroredDotVisible = if (2 * x - col < width) grid(row)(2 * x - col) else false
          newGrid.grid(row)(col) = grid(row)(col) || mirroredDotVisible
        }

        newGrid
    }
    result.printIt()
    result
  }

  /**
   * Execute multiple folds.
   *
   * @param foldInstructions the list of fold instructions.
   * @return a new grid.
   */
  def fold(foldInstructions: List[FoldLine]): Grid = {
    foldInstructions.foldLeft(this)((grid, foldLine) => grid.fold(foldLine))
  }

  /**
   * Count the number of visible dots.
   *
   * @return the number of visible dots
   */
  def visibleDots: Long = {
    grid.foldLeft(0L)((acc, row) => acc + row.count(dot => dot))
  }

  /**
   * Print the grid.
   *
   * A visible dot is represented by a <code>#</code>, an invisible on by a <code>.</code> (period).
   */
  def printIt(): Unit = {
    grid.foreach(row => {
      print(row.map(b => if (b) "#" else ".").mkString)
      println
    })
    println
  }

}

/**
 * Companion object.
 */
object Grid {

  /**
   * Creates a [[Grid]] from a list of points.
   *
   * First, the maximum coordinates are determined, so the dimensions are known.
   * Then, the grid is initialized with the points.
   *
   * @param points The visible points
   */
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
    def addNextCoordinate(acc: List[(Int, Int)], toParse: List[String]): List[(Int, Int)] = {

      toParse match {
        case Nil => acc
        case x::xs =>
          x match {
            // Stop at the first empty line
            case "" => acc
            case linePattern(x, y) => addNextCoordinate((x.toInt, y.toInt) :: acc, xs)
          }
      }
    }

    addNextCoordinate(List(), lines)
  }

  def parseFoldInstructions(lines: List[String]): List[FoldLine] = {
    val linePattern = "fold along (x|y)=(\\d*)".r

    @tailrec
    def iter(acc: List[FoldLine], toParse: List[String]): List[FoldLine] = {

      toParse match {
        case Nil => acc
        case x::xs =>
          x match {
            case linePattern(direction, value) => direction match {
              case "x" => iter(acc appended VFold(value.toInt), xs)
              case "y" => iter(acc appended HFold(value.toInt), xs)
            }
            // Continue if no match, i.e. skip all lines that aren't folding instructions, which are at the bottom of the file.
            case _ => iter(acc, xs)
          }
      }
    }

    iter(List(), lines)
  }

  val lines = readInput(getClass.getResource("input").getFile)
  val coordinates = parseCoordinates(lines)
  val foldInstructions = parseFoldInstructions(lines)

  val grid = Grid.create(coordinates)

  val result = grid.fold(foldInstructions.head)

  result.printIt()
  println("Visible lines after the first fold: " + result.visibleDots)

}
