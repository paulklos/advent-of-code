package day10

import day08.Day08a.readInput
import day10.Day10a.{parseLine, sumOfCorruptedLines}
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.{Assert, Test}

class Day10aTest {

  @Test
  def theEmptyStringLeadsToEnEmptyParseResult(): Unit = {
    val result = parseLine("")

    assertEquals(0, result.invalid)
    assertTrue(result.state.isEmpty)
  }

  @Test
  def leftParenthesesIsValidAndPutOnTheStack(): Unit = {
    val result = parseLine("(")

    assertFalse(result.isCorrupted)
    assertTrue(result.isIncomplete)
    assertEquals('(', result.state.top)
  }

  @Test
  def leftSquareBracketIsValidAndPutOnTheStack(): Unit = {
    val result = parseLine("[")

    assertFalse(result.isCorrupted)
    assertTrue(result.isIncomplete)
    assertEquals('[', result.state.top)
  }

  @Test
  def leftAngularBracketIsValidAndPutOnTheStack(): Unit = {
    val result = parseLine("<")

    assertFalse(result.isCorrupted)
    assertTrue(result.isIncomplete)
    assertEquals('<', result.state.top)
  }

  @Test
  def leftCurlyBraceIsValidAndPutOnTheStack(): Unit = {
    val result = parseLine("{")

    assertFalse(result.isCorrupted)
    assertTrue(result.isIncomplete)
    assertEquals('{', result.state.top)
  }

  @Test
  def aSinglePairOfParenthesesIsValid(): Unit = {
    val result = parseLine("()")

    assertEquals(0, result.invalid)
    assertTrue(result.state.isEmpty)
  }

  @Test
  def aSinglePairOfSquareBracketsIsValid(): Unit = {
    val result = parseLine("[]")

    assertEquals(0, result.invalid)
    assertTrue(result.state.isEmpty)
  }

  @Test
  def aSinglePairOfCurlyBracesIsValid(): Unit = {
    val result = parseLine("{}")

    assertEquals(0, result.invalid)
    assertTrue(result.state.isEmpty)
  }

  @Test
  def aSinglePairOfAngularBracketsIsValid(): Unit = {
    val result = parseLine("<>")

    assertEquals(0, result.invalid)
    assertTrue(result.state.isEmpty)
  }

  @Test
  def openParenthesesAndClosedSquareBracketsIsInvalid(): Unit = {
    val result = parseLine("(]")

    assertEquals(']', result.invalid)
    assertEquals(1, result.state.size)
    assertEquals('(', result.state.top)
  }

  @Test
  def theValueOfAnUnexpectedRightParenthesesIs3(): Unit = {
    val result = ParseResult(invalid = ')')

    assertEquals(3, result.value)
  }

  @Test
  def theValueOfAnUnexpectedRightSquareBracketIs57(): Unit = {
    val result = ParseResult(invalid = ']')

    assertEquals(57, result.value)
  }

  @Test
  def theValueOfAnUnexpectedRightCurlyBraceIs1197(): Unit = {
    val result = ParseResult(invalid = '}')

    assertEquals(1197, result.value)
  }

  @Test
  def theValueOfAnUnexpectedRightAngularBracketIs25137(): Unit = {
    val result = ParseResult(invalid = '>')

    assertEquals(25137, result.value)
  }

  @Test
  def corrupted1(): Unit = {
    val result = parseLine("{([(<{}[<>[]}>{[]{[(<()>")

    assertEquals('}', result.invalid)
    assertEquals(6, result.state.size)
    assertEquals('[', result.state.top)
  }

  @Test
  def corrupted2(): Unit = {
    val result = parseLine("[[<[([]))<([[{}[[()]]]")

    assertEquals(')', result.invalid)
    assertEquals(4, result.state.size)
    assertEquals('[', result.state.top)
  }

  @Test
  def theSumOfTheExampleIs26397(): Unit = {
    val lines = readInput(getClass.getResource("testInput").getFile)

    val result = sumOfCorruptedLines(lines)

    assertEquals(26397, result)
  }

}
