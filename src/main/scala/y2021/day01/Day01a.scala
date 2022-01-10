package y2021.day01

import scala.io.Source

object Day01a extends App {

  def doIt(totals: Increments, current: String) : Increments = {
    println(current)
    val currentInt = current.toInt
    println(currentInt)
    if (totals.lastValue > 0 && currentInt > totals.lastValue) Increments(currentInt, totals.numIncrements + 1)
    else Increments(currentInt, totals.numIncrements)
  }

  case class Increments(lastValue: Int, numIncrements: Int)

  val file = getClass.getResource("input").getFile
  val lines = Source.fromFile(file)

  val result = lines.getLines().foldLeft(Increments(0,0))(doIt)
  lines.close()

  println(result)
}
