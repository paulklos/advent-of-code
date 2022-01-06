package day15

import day08.Day08a
import day09.Day09a

import scala.annotation.tailrec

object Day15b extends App {

  def multiplyGrid(risks: List[List[Int]]): List[List[Int]] = {
    def addToList(l: List[Int], term: Int): List[Int] = {
      l.map(x => if (x + term >= 10) x + term - 9 else x + term)
    }

    (1 to 4).foldLeft(risks)((acc, term) => {
      risks.indices.foldLeft(acc)((acc, row) => {
        acc.appended(addToList(risks(row), term))
      })
    }).map(l => {
      (0 to 4).foldLeft(List[Int]())((acc, term) => {
        acc ::: addToList(l, term)
      })
    })
  }


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
     * The algorithm is pretty much the same as the one in [[Day15a]] but we're allowing steps in four directions,
     * instead of two. So we can also go up or left. The consequence is that we need to filter the results,
     * excluding all points to which the route has a higher risk than the current best route to the end.
     *
     * @param acc the accumulator, holds the route with the lowest risk per Position.
     * @return an new accumulator
     */
    @tailrec
    def step(acc: Map[Position, Route]): Map[Position, Route] = {

      // Calculate the positions where we can go
      def nextPositions(position: Position): List[Position] = {
        val newPos = position match {
          case Position(MAX_ROW, MAX_COL) => List()
          case Position(row, col) => List(Position(row + 1, col), Position(row - 1, col), Position(row, col - 1), Position(row, col + 1))
        }
        // Keep it within the grid
        newPos.filter(p => p.row >= 0 && p.col >= 0).filter(p => p.row < HEIGHT && p.col < WIDTH).filter(p => p != position)
      }

      // Now, fold left on the keys of the original accumulator to create a new accumulator
      val result = acc.keys.foldLeft(acc.filter(entry => entry._1 == Position(MAX_ROW, MAX_COL)))((newAcc, pos) => {
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

      // Filter first
      val filteredResult = result.get(Position(MAX_ROW, MAX_COL)) match {
        case None =>
          println(f"No path yet, ${result.size} paths")
          result
        case Some(minRisk) =>
          println(f"Route with lowest risk has ${minRisk.steps.size} steps and risk ${minRisk.risk}")
          val tmp = result.filter(entry => entry._2.risk < minRisk.risk)
          println(f"Found ${result.size} routes. Filtered to ${tmp.size}")
          tmp + (Position(MAX_ROW, MAX_COL) -> minRisk)
      }

      // Termination condition, only the end position is left
      if (filteredResult.keys.size == 1 && filteredResult.keys.head == Position(MAX_ROW, MAX_COL)) filteredResult
      // Otherwise, rinse and repeat
      else step(filteredResult)
    }

    step(Map[Position, Route](Position.START -> Route(List(Position.START)))).values.head
  }

  def printGrid(risks: List[List[Int]]): Unit = {
    risks.foreach(l => println(l.mkString("")))
  }

  val lines = Day08a.readInput(getClass.getResource("input").getFile)

  val risks = Day09a.parseLines(lines)
  val multi = multiplyGrid(risks)
  //printGrid(multi)
  val result = lowestRiskRoute(multi)

  println("The risk of the lowest risk route is: " + result.risk)
}
