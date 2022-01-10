package y2021.day03

import y2021.day03.Day03a.{BitCount, count}

import java.util.function.Predicate
import scala.annotation.tailrec
import scala.io.Source

object Day03b extends App {

  def filter(readings: List[String], filterValue: Char, position: Int): List[String] = {
    println("Filtering for " + filterValue + " in position " + position)
    readings.filter(value => value(position) == filterValue)
  }

  /*
   * Recursively loop over the list of readings, until there's only one left, which is the result.
   *
   * First, the bit counts of the list are determined, then the predicate is applied to the bit count
   * for the given position.
   *
   * Depending on the predicate, the list is further filtered for either the filterValue or its inverse in the given position.
   */
  @tailrec
  def find(predicate: Predicate[BitCount], filterValue: Char, inverse: Char)(readings: List[String], position: Int): String = {

    println(readings)

    readings match {
      // Single result left
      case x::Nil => x
      case _::_ =>
        // Determine the bit counts
        val bitCounts = readings.foldLeft(Nil: List[BitCount])(count)
        println(bitCounts)
        val bitCount = bitCounts(position)
        if (predicate.test(bitCount)) {
          find(predicate, filterValue, inverse)(filter(readings, filterValue, position), position + 1)
        }
        else {
          find(predicate, filterValue, inverse)(filter(readings, inverse, position), position + 1)
        }
    }
  }

  val file = getClass.getResource("input").getFile
  val lines = Source.fromFile(file)
  val linesList = lines.getLines().toList

  lines.close()

  val oxy = find(bitCount => bitCount.moreOnes, '1', '0')(linesList, 0)
  val co2 = find(bitCount => bitCount.fewerZeros, '0', '1')(linesList, 0)

  println("Oxy: " + oxy)
  println("CO2: " + co2)

  val oxyDec = Integer.parseInt(oxy, 2)
  val co2Dec = Integer.parseInt(co2, 2)
  println("Oxy (Dec): " + oxyDec)
  println("CO2 (Dec): " + co2Dec)

  println("Life support rating: " + oxyDec * co2Dec)
}
