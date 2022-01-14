package y2021.day17

import y2021.day08.Day08a

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.matching.Regex

case class Point(x: Int, y: Int) {

  def move(velocity: Velocity): Point = {
    Point(x + velocity.x, y + velocity.y)
  }

}

case class Velocity(x: Int, y: Int) {

  def next: Velocity = x match {
    case 0 => Velocity(x, y - 1)
    case _ if x > 0 => Velocity(x - 1, y - 1)
    case _ => throw new IllegalStateException
  }

  def incVertical: Velocity = Velocity(x, y + 1)
}

case class Trajectory(route: List[Point], velocity: Velocity) {

  def next: Trajectory = {
    Trajectory(route.head.move(velocity) :: route, velocity.next)
  }

  def highest: Point = route.sorted(Trajectory.HeightOrdering).reverse.head
}

object Trajectory {
  def withVelocity(velocity: Velocity): Trajectory = Trajectory(List(Point(0,0)), velocity)

  object HeightOrdering extends Ordering[Point] {
    def compare(a: Point, b: Point): Int = a.y compare b.y
  }
}

/**
 * An Area represents a rectangular area modeled by the top-left and the bottom-right corner.
 */
case class Area(topLeft: Point, bottomRight: Point) {

  /**
   * Determin if the point is within the area.
   *
   * @param point the point to check
   * @return true if the point is in the area
   */
  def isInside(point: Point): Boolean = {
    point match {
      case Point(x, _) if x < topLeft.x => false
      case Point(x, _) if x > bottomRight.x => false
      case Point(_, y) if y > topLeft.y => false
      case Point(_, y) if y < bottomRight.y => false
      case _ => true
    }
  }

  /**
   * Determine if a point is over the area.
   *
   * Over means right of the right-hand edge, or below the bottom edge. Either means a trajectory ending in this point can never end
   * inside the area anymore.
   *
   * @param point the point to check
   * @return true if the point is over the area
   */
  def isOver(point: Point): Boolean = {
    point match {
      case Point(x, y) if x > bottomRight.x || y < bottomRight.y => true
      case _ => false
    }
  }

  /**
   * Find the trajectory that ends inside the area, and reaches the highest vertical value along the way.
   *
   * We use the Option because initially we won't have a result yet. Only when the first trajectory lands
   * inside the area, will we have something to compare to.
   *
   * @return an [[Option]] of the [[Trajectory]]
   */
  def findHighestTrajectoryTo: Option[Trajectory] = {
    /**
     * The highest possible velocity in horizontal direction.
     * An velocity higher than this will overshoot the area in the first step.
     */
    val MAX_VX = bottomRight.x

    /**
     * The highest possible vertical velocity.
     * Assuming a positive vertical velocity, the probe will first go up, then down, and pass y == 0 with the
     * starting vertical velocity, only negative. The fist step below y == 0 will have a vertical velocity of
     * -(vx-initial + 1). If that lands us below the area, the area can never be reached anymore.
     *
     * This poses an upper bound on the vertical velocity.
     */
    val MAX_VY = 1 - bottomRight.y

    /**
     * Determine the trajectory that lands in the target area and reaches the greatest height.
     *
     * @param best the best trajectory so far, if any
     * @param trajectory the trajectory to evaluate
     * @param initial the initial velocity, used to start the next iteration when the current trajectory ends
     * @return an Option containing the best trajectory, if one was found
     */
    @tailrec
    def iter(best: Option[Trajectory], trajectory: Trajectory, initial: Velocity): Option[Trajectory] = {

      // The current trajectory ends if it's a hit or if it passes the target area
      val terminate = isInside(trajectory.route.head) || isOver(trajectory.route.head)
      // See if we have a winner
      val newBest = if (isInside(trajectory.route.head)) {
        println("Hit! Initial velocity was: " + initial)
        best match {
          // We already had a best, so compare
          case Some(b) => if (trajectory.highest.y > b.highest.y) Option(trajectory) else best
          // No hit yet, so the current trajectory is best by default
          case None => Option(trajectory)
        }
      } else best

      if (terminate) {
        initial match {
          // Max vx reached, the end
          case Velocity(MAX_VX, _) => newBest
          case Velocity(x, MAX_VY) =>
            // Max Y reached, increase vx
            val newV = Velocity(x + 1, 0)
            iter(newBest, Trajectory.withVelocity(newV), newV)
          case v: Velocity =>
            // Try the next vy
            val newV = v.incVertical
            iter(newBest, Trajectory.withVelocity(newV), newV)
        }
      } else {
        // Next step on the current trajectory
        iter(newBest, trajectory.next, initial)
      }
    }

    /*
     * It doesn't make sens to start with values that are too small to reach the target area.
     * The reach ov initial velocity vx is (vx * (vx + 1)) / 2, which is the sum of the range of
     * values from 1 to vx. This works out to a quadratic equation which yields the minimum vx
     * required.
     */
    val vx = Math.round((Math.sqrt(8 * topLeft.x) - 1) / 2).toInt

    // We don't even try negative vy, because we're looking for the highest y
    val initialVelocity = Velocity(vx, 0)
    iter(Option.empty, Trajectory.withVelocity(initialVelocity), initialVelocity)
  }
}

object Area {
  // Regex to parse the target area definition
  val format: Regex = """^target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)$""" r

  /**
   * Parse the target area definition string into an Area.
   *
   * @param input the target area string
   * @return the area
   */
  def parse(input: String): Area = {
    input match {
      case format(x0, x1, y0, y1) => Area(Point(x0.toInt, y1.toInt), Point(x1.toInt, y0.toInt))
    }
  }
}

object Day17a extends App {

  val input = Day08a.readInput(getClass.getResource("input").getFile).head
  val area = Area.parse(input)
  area.findHighestTrajectoryTo match {
    case Some(t) =>
      println("Found trajectory: " + t)
      println("Highest point: " + t.highest)
    case None => println("No trajectory found!")
  }
}
