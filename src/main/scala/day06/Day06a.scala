package day06

import scala.annotation.tailrec
import scala.io.Source

object Day06a extends App {

  def addDay(lanternFish: List[Int]): List[Int] = {

    @tailrec
    def spawn(spawned: List[Int], newPopulation: List[Int], population: List[Int]): List[Int] = {
      population match {
        case Nil =>
          newPopulation ::: spawned
        case x :: xs =>
          val newDays = if (x > 0) x - 1 else 6
          val newSpawned = if (x == 0) 8 :: spawned else spawned
          spawn(newSpawned, newDays :: newPopulation, xs)
      }
    }
    spawn(List(), List(), lanternFish)
  }

  @tailrec
  def addDays(days: Int, population: List[Int]): List[Int] = {

    if (days == 0) {
      println("Last Day")
      population
    } else {
      println("Day " + days)
      println("Population: " + population)
      addDays(days - 1, addDay(population))
    }
  }

  def readPopulation(file: String): List[Int] = {
    val populationSource = Source.fromFile(file)

    val populationLines = populationSource.getLines().toList

    populationSource.close()

    populationLines
      .filter(numbers => numbers.nonEmpty)
      .flatMap(numbers => numbers.split(","))
      .map(number => number.toInt)
  }

  val lanternFish = readPopulation(getClass.getResource("input").getFile)

  val day80 = addDays(80, lanternFish)

  println("Population size after 80 days: " + day80.size)

  val day256 = addDays(176, day80)
  println("Population size after 256 days: " + day256.size)
}
