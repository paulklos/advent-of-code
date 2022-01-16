package y2021.day17

import y2021.day08.Day08a

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.matching.Regex

object Day17b extends App {

  val input = Day08a.readInput(getClass.getResource("input").getFile).head
  val area = Area.parse(input)

  val velocities = area.possibleVelocities()
  println("Found velocities: " + velocities.size)

}
