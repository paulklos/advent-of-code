package day13

import day08.Day08a.readInput
import day13.Day13a.{parseCoordinates, parseFoldInstructions}

object Day13b extends App {

  val lines = readInput(getClass.getResource("input").getFile)
  val coordinates = parseCoordinates(lines)
  val foldInstructions = parseFoldInstructions(lines)

  val grid = Grid.create(coordinates)

  // Just execute all the fold instructions
  val result = grid.fold(foldInstructions)

  result.printIt()
  println("Visible lines after all the folds: " + result.visibleDots)

}
