package day10

import day08.Day08a.readInput
import day10.ParseResult.{closingMatches, values}

import scala.collection.mutable

case class ParseResult(state: mutable.Stack[Char] = mutable.Stack(), invalid: Char = 0) {

  def getCompletionString: String = {
    state.foldLeft("")((acc, c) => acc + closingMatches(c))
  }

  def isCorrupted: Boolean = {
    invalid > 0
  }

  def isIncomplete: Boolean = {
    state.nonEmpty
  }

  def value: Long = {
    values(invalid)
  }

}

object ParseResult {

  // Maps closing brackets to their corresponding opening bracket
  val openingMatches: Map[Char, Char] = Map(')' -> '(', '}' -> '{', ']' -> '[', '>' -> '<')

  // Maps opening brackets to their corresponding closing bracket
  val closingMatches: Map[Char, Char] = for ((k, v) <- openingMatches) yield v -> k

  // Error value of the unexpected characters
  val values: Map[Char, Long] = Map(')' -> 3L, '}' -> 1197L, ']' -> 57L, '>' -> 25137L)

  // Scoring value of the completion characters
  val completionValues: Map[Char, Long] = Map(')' -> 1L, ']' -> 2L, '}' -> 3L, '>' -> 4L)

  def isLeft(c: Char): Boolean = {
    "({[<".contains(c)
  }

  def isRight(c: Char): Boolean = {
    ")}]>".contains(c)
  }

  def matchingLeft(c: Char): Char = {
    openingMatches(c)
  }

}

object Day10a extends App {

  def parseLine(line: String): ParseResult = {

    line.foldLeft(ParseResult())((acc, c) => {
      // Skip if the line is already corrupted
      if (acc.isCorrupted) acc
      else {
        // Left bracket, i.e. opening -> push to stack
        if (ParseResult.isLeft(c)) {
          println("Pushing " + c)
          ParseResult(acc.state.push(c))
        }
        // Closing bracket
        else if (ParseResult.isRight(c)) {
          if (acc.state.top == ParseResult.matchingLeft(c)) {
            // Matches
            println("Popping " + acc.state.pop + ", stack size is now " + acc.state.size)
            ParseResult(acc.state)
          } else {
            // Corrupted
            println("Invalid character found: " + c)
            ParseResult(acc.state, c)
          }
        } else throw new IllegalArgumentException
      }
    })
  }

  def sumOfCorruptedLines(lines: List[String]): Long = {
    lines.map(line => parseLine(line))
      .filter(pr => pr.isCorrupted)
      .map(pr => pr.value)
      .sum
  }
  val lines = readInput(getClass.getResource("input").getFile)

  println("The sum of the corrupted lines is: " + sumOfCorruptedLines(lines))

}
