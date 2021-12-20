package day07

import day06.Day06a.readPopulation
import day06.Day06b.toMap

import java.lang.Integer.{MAX_VALUE, MIN_VALUE}
import scala.annotation.tailrec

object Day07a extends App {

  def determineRange(values: List[Int]): (Int, Int) = {

    @tailrec
    def rangeIter(acc: (Int, Int), toTest: List[Int]): (Int, Int) = {
      toTest match {
        case Nil => acc
        case x::xs =>
          println(f"Current range is $acc, testing $x: $x < ${acc._1} = ${x < acc._1}, $x > ${acc._2} = ${x > acc._2}")
          val newMin = if (x < acc._1) x else acc._1
          val newMax = if (x > acc._2) x else acc._2
          val newAcc = (newMin, newMax)
          println(f"New range is $newAcc")
          rangeIter(newAcc, xs)
      }
    }

    rangeIter((MAX_VALUE, MIN_VALUE), values)
  }

  def determineCostPerPosition(fPositionCost: (Int, Int) => Long)(positions: List[Int]): Map[Int, Long] = {
    val frequencies = toMap(Map(), positions)
    val range = determineRange(frequencies.keys.toList)

    @tailrec
    def iter(position: Int, acc: Map[Int, Long]): Map[Int, Long] = {
      if (position > range._2) acc
      else {
        val positionCost = frequencies.keys.foldLeft(0L)((acc, key) => {
          acc + fPositionCost(key, position) * frequencies(key)
        })
        iter(position + 1, acc + (position -> positionCost))
      }
    }

    iter(range._1, Map())
  }

  def determineFixedCostPerPosition(positions: List[Int]): Map[Int, Long] = {
    determineCostPerPosition((key, position) => Math.abs(key - position))(positions)
  }

  val positions = readPopulation(getClass.getResource("input").getFile)
  println(positions)

  val frequencies = toMap(Map(), positions)
  println(frequencies)

  val positionRange = determineRange(frequencies.keys.toList)
  println(positionRange)

  val costPerPosition = determineFixedCostPerPosition(positions)
  println(costPerPosition.minBy(_._2))

}
