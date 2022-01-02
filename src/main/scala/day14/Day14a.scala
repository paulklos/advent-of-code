package day14

import day08.Day08a

import scala.annotation.tailrec
import scala.language.postfixOps

case class Polymer(sequence: String) {

  def replace(rules: Map[String, Char]): Polymer = {
    Polymer(
      sequence
        .sliding(2)
        .map(s => {
          if (rules.contains(s))
            s.substring(0, 1) + rules(s) + s.substring(1)
          else s
        })
        .foldLeft("")((acc, s) =>
          if (acc.isBlank)
            acc + s
          else
            acc + s.substring(1)))
  }

  def replace(rules: Map[String, Char], times: Int): Polymer = {

    @tailrec
    def iter(acc: Polymer, timesLeft: Int): Polymer = {
      if (timesLeft == 0) acc
      else iter(acc.replace(rules), timesLeft - 1)
    }

    iter(this, times)
  }

  def frequencies: Map[Char, Int] = {
    sequence.foldLeft(Map[Char, Int]())((acc, c) => acc + (c -> (acc.getOrElse(c, 0) + 1)))
  }

  def mostFrequentMinusLeastFrequent: Int = {
    val freq = frequencies.values.toList.sorted
    freq.last - freq.head
  }
}

object Day14a extends App {

  def parseInsertionRules(lines: List[String]): Map[String, Char] = {

    @tailrec
    def addNextRule(acc: Map[String, Char], toParse: List[String]): Map[String, Char] = {

      val linePattern = """([A-Z]{2}) -> ([A-Z])"""r

      toParse match {
        case Nil => acc
        case x::xs =>
          x match {
            case linePattern(pair, insertion) =>
              //println(f"Found pair $pair with insertion $insertion")
              addNextRule(acc + (pair -> insertion(0)), xs)
            // Skip anything unknown
            case x =>
              println("Not an insertion rule, skipping: " + x)
              addNextRule(acc, xs)
          }
      }
    }

    addNextRule(Map(), lines)
  }

  val lines = Day08a.readInput(getClass.getResource("input").getFile)
  val template = lines.head
  val insertionRules = parseInsertionRules(lines)

  println("Found template: " + template)
  println("Found insertionRules: " + insertionRules)

  val polymer = Polymer(template)
  // Apply the rules 10 times
  val result = polymer.replace(insertionRules, 10)
  println("Difference: " + result.mostFrequentMinusLeastFrequent)
}
