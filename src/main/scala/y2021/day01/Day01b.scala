package y2021.day01

import scala.io.Source

object Day01b extends App {

  case class Increments(lastValue: Int, numIncrements: Int)

  def doIt(totals: Increments, current: Seq[Int]) : Increments = {
    println(current)
    val currentInt = current.sum
    println(currentInt)
    val result = {
      if (totals.lastValue > 0 && currentInt > totals.lastValue) {
        Increments(currentInt, totals.numIncrements + 1)
      } else {
        Increments(currentInt, totals.numIncrements)
      }
    }

    println(result)
    result
  }

  val file = getClass.getResource("input").getFile
  val lines = Source.fromFile(file)

  val result = lines.getLines().map(c => c.toInt)

  val grouped = result.sliding(3)
  val endResult = grouped.foldLeft(Increments(0,0))(doIt)


  println(endResult)
  lines.close()
}
