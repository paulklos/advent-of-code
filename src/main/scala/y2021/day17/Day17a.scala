package y2021.day17

import y2021.day08.Day08a

import java.math.{MathContext, RoundingMode}
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

object Velocity {

  /**
   * Calculate the minimum velocity required to reach the area, given the x-velocity.
   *
   * Given that the y-velocity decreases by 1 every step, the minimum y velocity required is
   * the number of steps plus the y value of the bottom edge of the area.
   *
   * @param vx the x velocity
   * @param area the area
   * @return the velocity with the lowest y velocity that will still reach the area
   */
  def min(vx: Int, area: Area): Velocity = {

    @tailrec
    def iter(steps: Int, x: Int, vx: Int): Int = {
      if (x >= area.topLeft.x) steps
      else {
        // This vx was too low to reach the target area
        if (vx == 0) throw new IllegalArgumentException
        else iter(steps + 1, x + vx, vx - 1)
      }
    }
    val steps = iter(0, 0, vx)
    Velocity(vx, steps + area.bottomRight.y - 1)
  }

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
   * Any velocity greater than or equal to this will overshoot the area in the first step.
   *
   * This is used as a terminating condition.
   */
  val VX_OVER: Int = bottomRight.x + 1

  /**
   * The highest possible vertical velocity.
   * Assuming a positive vertical velocity, the probe will first go up, then down, and pass y == 0 with the
   * starting vertical velocity, only negative. The fist step below y == 0 will have a vertical velocity of
   * -(vx-initial + 1). If that lands us below the area, the area can never be reached anymore.
   *
   * This poses an upper bound on the vertical velocity.
   */
  val MAX_VY: Int = 1 - bottomRight.y

  /**
   * The minimum x velocity required to reach the target area.
   *
   * It doesn't make sense to start with values that are too small to reach the target area.
   * The reach ov initial velocity vx is (vx * (vx + 1)) / 2, which is the sum of the range of
   * values from 1 to vx. This works out to a quadratic equation which yields the minimum vx
   * required.
   */
  // Two decimals are needed for proper rounding.
  private val mc = new MathContext(2, RoundingMode.UP)
  private val MIN_VX = BigDecimal(Math.sqrt(8 * topLeft.x + 1) / 2, mc).intValue

  /**
   * Determin if the point is within the area.
   *
   * @param point the point to check
   * @return true if the point is in the area
   */
  def isInside(point: Point): Boolean = {
    point match {
      case Point(x, _) if x < topLeft.x || x > bottomRight.x => false
      case Point(_, y) if y < bottomRight.y || y > topLeft.y => false
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
          case Velocity(VX_OVER, _) => newBest
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

    // We don't even try negative vy, because we're looking for the highest y
    val initialVelocity = Velocity(MIN_VX, 0)
    iter(Option.empty, Trajectory.withVelocity(initialVelocity), initialVelocity)
  }

  /**
   * Determine all the velocities that land within this Area after any step.
   *
   * Iterate over the possible horizontal velocities, and for each of those,
   * over the possible vertical velocities.
   *
   * The ranges are determined by:
   * <ul>
   *   <li>The minimum horizontal direction required to reach this Area</li>
   *   <li>The maximum horizontal direction is one past the right-hand edge of the target area</li>
   *   <li>The minimum vertical velocity, see [[Velocity.min]]</li>
   *   <li>The maximum vertical velocity, see MAX_VY</li>
   * </ul>
   * @return the velocities
   */
  def possibleVelocities(): List[Velocity] = {

    /**
     * Add the next step to the given Trajectory until it either lands inside this Area, or misses it.
     *
     * In either of those two cases, the next vertical velocity is tried. If the maximum vertical velocity is reached,
     * the next horizontal velocity is tried. The process stops once the maximum horizontal velocity is reached.
     *
     * @param acc the list of Velocities that land inside this Area.
     * @param trajectory the Trajectory being evaluated.
     * @param initial the initial Velocity of the Trajectory
     * @return the list of Velocities
     */
    @tailrec
    def iter(acc: List[Velocity], trajectory: Trajectory, initial: Velocity): List[Velocity] = {
      // The current trajectory ends if it's a hit or if it passes the target area
      val terminate = isInside(trajectory.route.head) || isOver(trajectory.route.head)
      // See if we have a winner
      val newAcc = if (isInside(trajectory.route.head)) {
        println("Inside target area with initial velocity: " + initial)
        initial :: acc
      } else acc

      if (isOver(trajectory.route.head))
        println("Missed target area with initial velocity: " + initial)

      if (terminate) {
        initial match {
          // Max vx reached, the end
          case Velocity(VX_OVER, _) =>
            println(f"Max VX with initial velocity: $initial, terminate")
            newAcc
          case Velocity(x, MAX_VY) =>
            // Max Y reached, increase vx
            println(f"Max VY with initial velocity: $initial, next VX")
            val newV = Velocity.min(x + 1, this)
            iter(newAcc, Trajectory.withVelocity(newV), newV)
          case v: Velocity =>
            // Try the next vy
            val newV = v.incVertical
            iter(newAcc, Trajectory.withVelocity(newV), newV)
        }
      } else {
        // Next step on the current trajectory
        iter(newAcc, trajectory.next, initial)
      }
    }

    val initialVelocity = Velocity.min(MIN_VX, this)
    val velocities = iter(List(), Trajectory.withVelocity(initialVelocity), initialVelocity)
    println("Velocities found: " + velocities)
    velocities
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
