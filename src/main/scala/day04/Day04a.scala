package day04

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

class Card {

  // Mutable Sets for each row and column, so we can remove values when they're drawn
  private val rows = (for (x <- 0 to 4) yield new mutable.HashSet[Int]()).toList
  private val cols = (for (x <- 0 to 4) yield new mutable.HashSet[Int]()).toList

  // Index numbers on the card to the value: value -> coordinates (row, col)
  private val valueMap: mutable.Map[Int, (Int, Int)] = mutable.Map()

  def addRow(row: Int, line: String): Unit = {

    @tailrec
    def addValue(row: Int, col: Int, rowValues: List[Int]): Unit = {

      def setValue(value: Int): Unit = {
        valueMap.put(value, (row, col))
        rows(row).add(value)
        cols(col).add(value)
      }

      rowValues match {
        case x::Nil =>
          setValue(x)
        case x::xs =>
          setValue(x)
          addValue(row, col + 1, xs)
      }
    }
    println(f"Adding row '$line'")
    addValue(row, 0, line.trim.split("\\s+").map(s => {
      println(f"Input value is '$s'")
      s.toInt
    }
    ).toList)
  }

  def draw(value: Int): Int = {
    // Locate the number on the card
    val coordinates = valueMap.remove(value)
    if (coordinates.isDefined) {
      // It's on the card
      val row = rows(coordinates.get._1)
      val col = cols(coordinates.get._2)
      row.remove(value)
      col.remove(value)
      if (row.isEmpty || col.isEmpty)
        // bingo
        valueMap.keys.sum * value
      else
        0
    } else {
      0
    }
  }

}

object Day04a extends App {

  @tailrec
  def readCardLines(acc: List[Card], lines: List[String], row: Int): List[Card] = {
    lines match {
      case x::xs =>
        println(f"$row%02d $x")
        val nextRow = if (x.isBlank) 0 else row + 1
        if (nextRow == 0)
          readCardLines(new Card()::acc, xs, nextRow)
        else {
          acc.head.addRow(row, x)
          readCardLines(acc, xs, nextRow)
        }
      case Nil => acc
    }
  }

  def readAllCards(): List[Card] = {
    val file = getClass.getResource("cards").getFile
    val lines = Source.fromFile(file)
    val linesList = lines.getLines().toList

    lines.close()

    readCardLines(List(new Card()), linesList, 0).reverse
  }

  val cards = readAllCards()

  def readDraw(): List[Int] = {
    val drawsFile = getClass.getResource("draw").getFile
    val draws = Source.fromFile(drawsFile)

    val draw = draws.getLines().toList

    draws.close()

    draw
      .filter(numbers => numbers.nonEmpty)
      .flatMap(numbers => numbers.split(","))
      .map(number => number.toInt)
  }

  val numbers = readDraw()

  for (num <- numbers)
    for (card <- cards) {
      val score = card.draw(num)
      if (score > 0) {
        println(score)
        println(card)
      }
    }
}
