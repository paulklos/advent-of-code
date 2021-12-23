package day08

import day08.Day08b.{applyTranslation, decodeLine, decodeSignalPatterns, rightPartToNumber}
import org.junit.Assert.assertEquals
import org.junit.Test

class Day08bTest {

  @Test
  def aMappingIsAppliedCorrectly(): Unit = {
    val input = "cdfbe"

    val mapping = Map('d' -> 'a', 'e' -> 'b', 'a' -> 'c', 'f' -> 'd', 'g' -> 'e', 'b' -> 'f', 'c' -> 'g')

    val result = applyTranslation(input, mapping)

    assertEquals("abdfg", result)
  }

  @Test
  def givenAMappingTheRightPartIsDecodedCorrectly(): Unit = {
    val input = "cdfeb fcadb cdfeb cdbaf"

    val mapping = Map('d' -> 'a', 'e' -> 'b', 'a' -> 'c', 'f' -> 'd', 'g' -> 'e', 'b' -> 'f', 'c' -> 'g')

    val result = rightPartToNumber(input, mapping)

    assertEquals(5353, result)
  }

  @Test
  def theSignalWiresAreDecoded(): Unit = {
    val input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"

    val mapping = Map('d' -> 'a', 'e' -> 'b', 'a' -> 'c', 'f' -> 'd', 'g' -> 'e', 'b' -> 'f', 'c' -> 'g')

    val result = decodeSignalPatterns(input)

    assertEquals(mapping, result)
  }

  @Test
  def example(): Unit = {
    val lines = List(
      "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf",
      "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |fgae cfgab fg bagce")

    val decoded = lines.map(decodeLine)

    assertEquals(List(5353, 8394, 9781, 1197, 9361, 4873, 8418, 4548, 1625, 8717, 4315), decoded)
  }
}
