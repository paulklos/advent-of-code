package y2021.day07

import y2021.day06.Day06a.readPopulation
import y2021.day06.Day06b.toMap
import y2021.day07.Day07a.{determineCostPerPosition, determineRange}

object Day07b extends App {

  def determineAscendingCostPerPosition(positions: List[Int]): Map[Int, Long] = {
    determineCostPerPosition((key, position) => {
      val diff = Math.abs(key - position)
      (diff * (diff + 1)) / 2
    })(positions)
  }

  val positions = readPopulation(getClass.getResource("input").getFile)
  println(positions)

  val frequencies = toMap(Map(), positions)
  println(frequencies)

  val positionRange = determineRange(frequencies.keys.toList)
  println(positionRange)

  val costPerPosition = determineAscendingCostPerPosition(positions)
  println(costPerPosition.minBy(_._2))

}
