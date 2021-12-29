package day11

import day08.Day08a.readInput

object Day11b extends App {

  val lines = readInput(getClass.getResource("input").getFile)

  val octopuses = DumboOctopuses.create(lines)

  println("First step for simultaneous flashing: " + octopuses.firstSimultaneous)

}
