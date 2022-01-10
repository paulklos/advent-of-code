package y2021.day10

import y2021.day08.Day08a.readInput
import y2021.day10.Day10a.parseLine
import y2021.day10.Day10b.{calculateCompletionScore, middleCorrectionScore}
import org.junit.Assert.assertEquals
import org.junit.Test

class Day10bTest {

  @Test
  def theCompletionStringsAreCorrect(): Unit = {
    val lines = List(
      "[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "(((({<>}<{<{<>}{[]{[]{}",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "<{([{{}}[<[[[<>{}]]]>[]]"
    )
    val result = lines
      .map(line => parseLine(line))
      .map(pr => pr.getCompletionString)

    assertEquals(List("}}]])})]", ")}>]})", "}}>}>))))", "]]}}]}]}>", "])}>"), result)
  }

  @Test
  def TheValueOfTheCompletionStringsIsCorrect(): Unit = {
    val completions = List("}}]])})]", ")}>]})", "}}>}>))))", "]]}}]}]}>", "])}>")
    val result = completions.map(calculateCompletionScore)

    assertEquals(List(288957L, 5566L, 1480781L, 995444L, 294L), result)
  }

  @Test
  def theMiddleValueTheExampleIs288957(): Unit = {
    val lines = readInput(getClass.getResource("testInput").getFile)

    val result = middleCorrectionScore(lines)

    assertEquals(288957, result)
  }


}
