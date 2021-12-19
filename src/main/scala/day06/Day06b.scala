package day06

import scala.annotation.tailrec
import scala.io.Source

object Day06b extends App {

  def addDay(population: Map[Int, Long]): Map[Int, Long] = {

    @tailrec
    def spawn(acc: Map[Int, Long], keys: List[Int]): Map[Int, Long] = {

      def newAcc(key: Int, value: Long): Map[Int, Long] = {
        val sixes = if (acc.contains(6)) acc(6) else 0
        print(f"Accumulator is $acc ")
        if (key > 0) {
          if (key == 7) {
            println(f"adding ${key -1} -> ${sixes + value}")
            acc + (key - 1 -> (sixes + value))
          } else {
            println(f"adding ${key -1} -> $value")
            acc + (key - 1 -> value)
          }
        }
        else {
          println(f"adding 6 -> ${sixes + value} and 8 -> $value")
          acc + (6 -> (sixes + value)) + (8 -> value)
        }
      }

      keys match {
        case x :: Nil =>
          newAcc(x, population(x))
        case x :: xs =>
          spawn(newAcc(x, population(x)), xs)
      }
    }

    spawn(Map(), population.keys.toList)
  }

  @tailrec
  def toMap(acc: Map[Int, Long], values: List[Int]): Map[Int, Long] = {

    def addValue(acc: Map[Int, Long], key: Int): Map[Int, Long] = {
      val lookup = acc.get(key)
      val newValue = if (lookup.nonEmpty) lookup.get + 1 else 1
      acc + (key -> newValue)
    }

    values match {
      case x::Nil => addValue(acc, x)
      case x::xs => toMap(addValue(acc, x), xs)
    }
  }

  @tailrec
  def addDays(days: Int, population: Map[Int, Long]): Map[Int, Long] = {

    if (days == 0) {
      println("Last Day")
      println("Population: " + population)
      population
    } else {
      println("Day " + days)
      println("Population: " + population)
      addDays(days - 1, addDay(population))
    }
  }

  val lanternFish = Day06a.readPopulation()

  val lookup = toMap(Map(), lanternFish)

  val day80 = addDays(80, lookup)

  println("Population size after 80 days: " + day80.values.sum)

  val day256 = addDays(256, lookup)
  println("Population size after 256 days: " + day256.values.sum)
}
