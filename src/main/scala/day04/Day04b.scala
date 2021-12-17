package day04

import scala.annotation.tailrec
import scala.io.Source

object Day04b extends App {

  val cards = Day04a.readAllCards()

  val drawsFile = getClass.getResource("draw").getFile
  val draws = Source.fromFile(drawsFile)

  val draw = draws.getLines().toList

  draws.close()

  val numbers = draw
    .filter(numbers => numbers.nonEmpty)
    .flatMap(numbers => numbers.split(","))
    .map(number => number.toInt)


  /*
   * Print out the score of the winning cards, in order of the numbers drawn.
   */
  @tailrec
  def drawNumbers(numbers: List[Int], cards: List[Card]): Unit = {
    numbers match {
      case x::xs =>
        /*
         * There are more numbers to follow.
         * Filter out the winning cards, and continue with the rest of the numbers
         */
        val filteredCards = cards.filter(card => {
          val score = card.draw(x)
          if (score > 0)
            println(score)
          score == 0
        })
        drawNumbers(xs, filteredCards)
      case Nil => // we're done

    }
  }

  drawNumbers(numbers, cards)
}