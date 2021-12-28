package day10

import day08.Day08a.readInput
import day10.ParseResult.values

import scala.collection.mutable

case class ParseResult(state: mutable.Stack[Char] = mutable.Stack(), invalid: Char = 0) {

  def isCorrupted: Boolean = {
    invalid > 0
  }

  def isIncomplete: Boolean = {
    state.nonEmpty
  }

  def value: Int = {
    values(invalid)
  }

}

object ParseResult {

  // Maps closing brackets to their corresponding opening bracket
  val matches: Map[Char, Char] = Map(')' -> '(', '}' -> '{', ']' -> '[', '>' -> '<')

  // Error value of the unexpected characters
  val values: Map[Char, Char] = Map(')' -> 3, '}' -> 1197, ']' -> 57, '>' -> 25137)

  def isLeft(c: Char): Boolean = {
    "({[<".contains(c)
  }

  def isRight(c: Char): Boolean = {
    ")}]>".contains(c)
  }

  def matchingLeft(c: Char): Char = {
    matches(c)
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
