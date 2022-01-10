package y2021.day11

import y2021.day08.Day08a.readInput

import scala.annotation.tailrec

case class DumboOctopuses(height: Int, width: Int) {

  private val grid: Array[Array[Int]] = Array.ofDim[Int](height, width)

  /**
   * Parse each line as a list of numbers and set these in the grid.
   *
   * @param lines the input lines
   */
  def fromInput(lines: List[String]): Unit = {
    for ((s, row) <- lines.zipWithIndex)
      for ((c, col) <- s.zipWithIndex)
        grid(row)(col) = c.asDigit
  }

  /**
   * Returns a list of all points in the grid.
   */
  def allPoints: List[(Int, Int)] = {
    (for (
      r <- 0 until height;
      c <- 0 until width) yield (r, c)).toList
  }

  /**
   * Execute a single step.
   *
   * First queue and increase of all the values in the grid.
   *
   * Process these, while adding new increases to the surrounding points if an octopus flashes.
   *
   * @return the number of flashes
   */
  def step: Int = {

    def surrounding(point: (Int, Int)): List[(Int, Int)] = {
      (for (r <- point._1 - 1 to point._1 + 1 if r >= 0 && r < height;
           c <- point._2 - 1 to point._2 + 1 if c >= 0 && c < width && (r, c) != point) yield (r, c)).toList
    }

    // Process the increases
    @tailrec
    def iter(increases: List[(Int, Int)]): Unit = {
      increases match {
        // done
        case Nil =>
        // increase the cell identified by x
        case x::xs =>
          val current = grid(x._1)(x._2)
          grid(x._1)(x._2) = current + 1
          if (current == 9) {
            // Flash, increase the surrounding points, i.e. add these to the increases to process
            val newPoints = surrounding(x)
            iter(xs ::: newPoints)
          } else iter(xs)
      }
    }

    printIt()

    // Start with simply increasing all the points and see what happens
    iter(allPoints)

    // Now there might be a values >9, which means these are octopuses that wil flash in this step
    val flashes = allPoints.foldLeft(0)((flashes, point) => {
      if (grid(point._1)(point._2) >= 10) {
        // Reset to zero
        grid(point._1)(point._2) = 0
        // Add flash
        flashes + 1
      } else
        flashes
    })

    printIt()

    flashes
  }

  def printIt(): Unit = {
    grid.foreach(row => {
      row.foreach(i => print(i + " "))
      println
    })
    println
  }

  // Run a specified number of steps
  def doSteps(steps: Int): Long = {

    @tailrec
    def iter(acc: Long, stepsToGo: Int): Long = {
      if (stepsToGo == 0) acc
      else iter(acc + step, stepsToGo - 1)
    }

    iter(0, steps)
  }

  def firstSimultaneous: Long = {

    @tailrec
    def iter(stepNo: Long): Long = {
      val flashes = step
      println(f"Step $stepNo, $flashes flashes.")
      if (flashes == width * height)
        stepNo
      else
        iter(stepNo + 1)
    }

    iter(1)
  }

  override def toString: String = {
    grid.foldLeft("")((acc, row) => row.foldLeft(acc)((acc, i) => acc + i))
  }

}

object DumboOctopuses {

  // Create a grid of Dumbo Octopuses with dimensions defined by the input
  def create(lines: List[String]): DumboOctopuses = {
    val rows = lines.size
    if (rows == 0) throw new IllegalArgumentException("No input")
    val cols = lines.head.length

    val result = DumboOctopuses(rows, cols)

    result.fromInput(lines)

    result
  }

}

object Day11a extends App {

  val lines = readInput(getClass.getResource("input").getFile)

  val octopuses = DumboOctopuses.create(lines)

  println("Total flashes after 100 steps: " + octopuses.doSteps(100))

}
