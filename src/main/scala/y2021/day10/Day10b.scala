package y2021.day10

import y2021.day08.Day08a.readInput
import y2021.day10.Day10a.parseLine

object Day10b extends App {

  def calculateCompletionScore(completion :String): Long = {
    completion.foldLeft(0L)((acc, c) => 5L * acc + ParseResult.completionValues(c))
  }

  def middleCorrectionScore(lines: List[String]): Long = {
    val scores = lines
      .map(parseLine)
      .filter(pr => !pr.isCorrupted)
      .filter(pr => pr.isIncomplete)
      .map(pr => pr.getCompletionString)
      .map(calculateCompletionScore)
      .sorted
    scores((scores.size - 1)/2)
  }

  val lines = readInput(getClass.getResource("input").getFile)

  println("The middle value of the correction values is: " + middleCorrectionScore(lines))

}
