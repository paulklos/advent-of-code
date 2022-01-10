package y2021.day14

import y2021.day08.Day08a
import y2021.day14.Day14a.parseInsertionRules
import y2021.day14.PolymerV2.incrementFrequency

import scala.annotation.tailrec
import scala.language.postfixOps

/**
 * Version 2 of the Polymer class.
 *
 * It holds the frequencies of each par and character, without storing the actual polymer string in memory.
 * Since a pair doubles in each step, a single pair would result in 2⁴40 pairs in 40 steps, which runs into the terabytes
 * for a single polymer.
 *
 * @param pairs the pairs and their frequency
 * @param freq the characters and their frequency
 */
case class PolymerV2(pairs: Map[String, Long], freq: Map[Char, Long]) {

  def replace(rules: Map[String, Char]): PolymerV2 = {

    // Apply all the correct rule to each pair
    // The initial accumulator is a tuple consisting of an empty map and the Polymer's frequency table
    // The map is empty because the pairs are completely reconstructed
    val updated = pairs.foldLeft((Map[String, Long](), freq))((acc, pair) => {
      pair match {
        case (value, frequency) =>
          // Get the insertion character for this pair
          rules.get(value) match {
            case None => acc
            case Some(charToAdd) =>
              // Use pattern matching on the accumulator to avoid having to use ._1 and ._2
              acc match {
                case (pairs, freqs) =>
                  //The original pair results in two new pairs
                  val newPair1 = value.substring(0, 1) + charToAdd
                  val newPair2 = charToAdd + value.substring(1)
                  // Update the pair frequencies in the accumulator
                  // Each new pair has the frequency of the original pair
                  (
                    pairs.concat(
                      Map(
                        newPair1 -> (pairs.getOrElse(newPair1, 0L) + frequency),
                        newPair2 -> (pairs.getOrElse(newPair2, 0L) + frequency)
                      )
                  ),
                    // Add the inserted character with the frequency of the original pair
                    incrementFrequency(freqs, charToAdd, frequency)
                  )
              }
          }
      }
    })
    // Return a new Polymer from the result
    updated match {
      case (pairFreq, charFreq) => PolymerV2(pairFreq, charFreq)
    }
  }

  def replace(rules: Map[String, Char], times: Int): PolymerV2 = {

    @tailrec
    def iter(acc: PolymerV2, timesLeft: Int): PolymerV2 = {
      println("Iterations left: " + timesLeft)
      println("Total # of pairs: " + acc.pairs.values.sum)
      if (timesLeft == 0) acc
      else iter(acc.replace(rules), timesLeft - 1)
    }

    iter(this, times)
  }

  def mostFrequentMinusLeastFrequent: Long = {
    val freq = this.freq.values.toList.sorted
    freq.last - freq.head
  }

}

object PolymerV2 {

  def create(template: String): PolymerV2 = {
    // Find the pairs in the template string by iterating with a sliding window of size 2
    val pairs = template
      .sliding(2)
      .foldLeft(Map[String, Long]())((acc, pair) => incrementFrequency(acc, pair, 1))
    val frequencies = template.foldLeft(Map[Char, Long]())((acc, c) => incrementFrequency(acc, c, 1))
    PolymerV2(pairs, frequencies)
  }

  def incrementFrequency[A](fm: Map[A, Long], c: A, inc: Long): Map[A, Long] = {
    fm + (c -> (fm.getOrElse(c, 0L) + inc))
  }

}

object Day14b extends App {

  val lines = Day08a.readInput(getClass.getResource("input").getFile)
  val template = lines.head
  val insertionRules = parseInsertionRules(lines)

  println("Found template: " + template)
  println("Found insertionRules: " + insertionRules)

  val polymer = PolymerV2.create(template)
  // Apply the rules 40 times
  val result = polymer.replace(insertionRules, 40)
  println("Difference: " + result.mostFrequentMinusLeastFrequent)
}
