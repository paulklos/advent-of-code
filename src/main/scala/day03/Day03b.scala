package day03

import java.util.function.Predicate
import scala.annotation.tailrec
import scala.io.Source

object Day03b extends App {

  case class BitCount(zero: Int, one: Int) {
    def moreOnes = one >= zero
    def moreZeros = zero <= one
  }

  def count(count: List[BitCount], input: String): List[BitCount] = {

    val single = input.foldLeft(List(): List[BitCount])((a: List[BitCount], c: Char) => {
      c.asDigit match {
        case 0 => a.appended(BitCount(1, 0))
        case 1 => a.appended(BitCount(0, 1))
      }
    })

    count match {
      case Nil => single
      case _ => count.zip(single).map(pair => BitCount(pair._1.zero + pair._2.zero, pair._1.one + pair._2.one))
    }
  }

  def filter(readings: List[String], filterValue: Char, position: Int): List[String] = {
    println("Filtering for " + filterValue + " in position " + position)
    readings.filter(value => value(position) == filterValue)
  }

  @tailrec
  def find(predicate: Predicate[BitCount], filterValue: Char, inverse: Char)(readings: List[String], position: Int): String = {

    println(readings)

    readings match {
      case x::Nil => x
      case _::_ =>
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
  val co2 = find(bitCount => bitCount.moreZeros, '0', '1')(linesList, 0)

  println("Oxy: " + oxy)
  println("CO2: " + co2)

  val oxyDec = Integer.parseInt(oxy, 2)
  val co2Dec = Integer.parseInt(co2, 2)
  println("Oxy (Dec): " + oxyDec)
  println("CO2 (Dec): " + co2Dec)

  println("Life support rating: " + oxyDec * co2Dec)
}
