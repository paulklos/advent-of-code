package y2021.day15

import y2021.day08.Day08a
import y2021.day09.Day09a

import scala.annotation.tailrec

case class Position(row: Int, col: Int)

object Position {
  val START: Position = Position(0, 0)
}

case class Route(steps: List[Position], risk: Long = 0) {
  def currentPosition: Position = steps.head
}

object Day15a extends App {

  /**
   * Calculate the route with the lowest risk from a grid of risks.
   *
   * @param risks the risk per Position in the grid
   * @return the route with the lowest risk
   */
  def lowestRiskRoute(risks: List[List[Int]]): Route = {

    val HEIGHT = risks.size
    val WIDTH = risks.head.size
    val MAX_ROW = HEIGHT - 1
    val MAX_COL = WIDTH - 1

    /**
     * Run a step, which means evaluate the routes where we can go from the routes in the accumulator.
     *
     * For each Position in the accumulator, the Positions are calculated where we can go next. This results
     * in new routes, starting with the current value in the accumulator (which is the value of the Position as the key).
     * Each of these routes end in a Position, and per position only the route with the lowest risk is kept, the others
     * are discarded.
     *
     * For example:
     * <pre>
     * The initial route starts at (0,0), which is the upper-left corner, and has an initial risk of zero.
     *
     * There are two possible directions: right and up.
     *
     * Resulting in two new routes:
     *
     * 1 - (0,0) - (0,1)
     * 1 - (0,0) - (1,0)
     *
     * The next step results in 4 routes
     *
     * Three are kept
     * 7 - (0,0) - (0,1) - (0,2)
     * 3 - (0,0) - (1,0) - (2,0)
     * 4 - (0,0) - (1,0) - (1,1)
     * And one is discarded, because there is a less risky route to get to (1,1)
     * 7 - (0,0) - (0,1) - (1,1)
     *
     * The next step now results in six (instead of eight) results
     *
     * 10 - (0,0) - (0,1) - (0,2) - (0,3)
     * 04 - (0,0) - (1,0) - (2,0) - (2,1)
     * 03 - (0,0) - (1,0) - (2,0) - (3,0)
     * 12 - (0,0) - (1,0) - (1,1) - (1,2)
     *
     * Of which an additional two can be discarded.
     * 15 - (0,0) - (0,1) - (0,2) - (1,2)
     * 05 - (0,0) - (1,0) - (1,1) - (2,1)
     * </pre>
     *
     * This means only a sort of expanding circle is kept in memory, which eventually narrows down to the end in the lower right corner.
     *
     * @param acc the accumulator, holds the route with the lowest risk per Position.
     * @return an new accumulator
     */
    @tailrec
    def step(acc: Map[Position, Route]): Map[Position, Route] = {

      // Calculate the positions where we can go
      def nextPositions(position: Position): List[Position] = {
        val newPos = position match {
          case Position(row, col) => List(Position(row + 1, col), Position(row, col + 1))
        }
        // Keep it within the grid
        newPos.filter(p => p.row < HEIGHT && p.col < WIDTH)
      }

      // Now, fold left on the keys of the original accumulator to create a new accumulator
      val result = acc.keys.foldLeft(Map[Position, Route]())((newAcc, pos) => {
        // Map the next position to a route, using the original route as a starting point and adding the next position and the risk
        nextPositions(pos).map(nextPos => {
          val route = acc(pos)
          Route(nextPos :: route.steps, route.risk + risks(nextPos.row)(nextPos.col))
        })
          // Fold left over the routes, keeping the one with the lowest risk for a position
          .foldLeft(newAcc)((a, route) => {
            a.updatedWith(route.currentPosition) {
              // New position, keep the new route
              case None => Some(route)
              // A route to this position already exists, keep the one with the lowest risk
              case Some(minRisk) => Some(if (route.risk < minRisk.risk) route else minRisk)
            }
        })
      })

      // Termination condition, only the end position is left
      if (result.keys.size == 1 && result.keys.head == Position(MAX_ROW, MAX_COL)) result
      // Otherwise, rinse and repeat
      else step(result)
    }

    step(Map[Position, Route](Position.START -> Route(List(Position.START)))).values.head
  }

  val lines = Day08a.readInput(getClass.getResource("input").getFile)

  val result = lowestRiskRoute(Day09a.parseLines(lines))
  println("The risk of the lowest risk route is: " + result.risk)
}
