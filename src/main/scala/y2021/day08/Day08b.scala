package y2021.day08

import y2021.day08.Day08a.readInput

/*
 *
 * Segment layout:
 *
 *   0:      1:      2:      3:      4:
 *  aaaa    ....    aaaa    aaaa    ....
 * b    c  .    c  .    c  .    c  b    c
 * b    c  .    c  .    c  .    c  b    c
 *  ....    ....    dddd    dddd    dddd
 * e    f  .    f  e    .  .    f  .    f
 * e    f  .    f  e    .  .    f  .    f
 *  gggg    ....    gggg    gggg    ....
 *
 *   5:      6:      7:      8:      9:
 *  aaaa    aaaa    aaaa    aaaa    aaaa
 * b    .  b    .  .    c  b    c  b    c
 * b    .  b    .  .    c  b    c  b    c
 *  dddd    dddd    ....    dddd    dddd
 * .    f  e    f  .    f  e    f  .    f
 * .    f  e    f  .    f  e    f  .    f
 *  gggg    gggg    ....    gggg    gggg
 *
 *
 * Segment -> digit:
 *
 * a -> 0, 2, 3, 5, 6, 7, 8, 9
 * b -> 0, 4, 5, 6, 8, 9
 * c -> 0, 1, 2, 3, 4, 7, 8, 9
 * d -> 2, 3, 4, 5, 6, 8, 9
 * e -> 0, 2, 6, 8
 * f -> 0, 1, 3, 4, 5, 6, 7, 8, 9
 * g -> 0, 2, 3, 5, 6, 8, 9
 *
 * Digit -> segment
 *
 * 0 -> abcefg
 * 1 -> cf
 * 2 -> acdeg
 * 3 -> acdfg
 * 4 -> bcdf
 * 5 -> abdfg
 * 6 -> abdefg
 * 7 -> acf
 * 8 -> abcdefg
 * 9 -> abcdfg
 *
 * Digit length:
 *
 * 0 -> 6
 * 1 -> 2
 * 2 -> 5
 * 3 -> 5
 * 4 -> 4
 * 5 -> 5
 * 6 -> 6
 * 7 -> 3
 * 8 -> 7
 * 9 -> 6
 *
 * Length frequencies:
 *
 * 2 -> 1
 * 3 -> 7
 * 4 -> 4
 * 5 -> 2, 3, 5
 * 6 -> 0, 6, 9
 * 7 -> 8
 *
 * 7 - 1 => a
 * There's a single 6-digit value that doesn't contain the segments of 1, which is 6.
 *
 * There is a single 5-digit value that contains all characters of the 7, which is 3.
 * 3 - 7 => dg
 * There's a single 6-digit value that doesn't contain the ´dg´ segments, which is 0.
 *
 * The difference between 0 and 8 is the 'd' segment.
 *
 * If we know 'd' we know 'g' as well: 3 - 7 - 'd'
 *
 * The remaining 6-digit values are 6 and 9. Only one of these contains 7, which is 9.
 * Obviously, that leaves the last one to be 6.
 *
 * We now also know segment 'b' 9 - 3
 *
 * The intersection of 6 and 1 yields 'c' and 'f'.
 *
 * Finally 'e' = 8 - 9.
 *
 * Now we also know which is 5 and which is 2.
 */
object Day08b extends App {

  def sevenSegmentCodes() = {
    Map("abcefg" -> 0, "cf" -> 1, "acdeg" -> 2, "acdfg" -> 3, "bcdf" -> 4, "abdfg" -> 5, "abdefg" -> 6, "acf" -> 7, "abcdefg" -> 8, "abcdfg" -> 9)
  }

  def containsAllChars(toTest: String, contained: String): Boolean = {
    toTest.intersect(contained).sorted == contained.sorted
  }

  def subtractString(s: String, toSubtract: String): String = {
    s filterNot (toSubtract contains _)
  }

  /**
   * Identifies the representation of each digit.
   *
   * There are some easy ones, because the '1', '4', '7' and '8' each use a unique number of segments: 2, 3, 4 and 7, respectively.
   *
   * By inspecting the differences and similarities between the known digit representations, the other representations can be
   * identified as well. For example, the '3' is the only representation of 5 segments, that contains all the segments of 7:
   * nether '2' nor '5' does.
   *
   * Note that in the end the '2' and '5' representations are not identified. It's not needed as the other digits
   * give enough information to identify all the segments.
   *
   * @param patterns a pattern string containing space-separated strings, each representing a digit of 0-9.
   * @return a map of digit -> representation
   */
  def parseSignals(patterns: String): Map[Int, String] = {
    val parts = patterns
      .split(" ")
      .map(s => s.trim)
      .map(s => s.sorted)

    def findByLength(value: Int, length: Int)(acc: Map[Int, String]): Map[Int, String] = {
      val result = parts.filter(s => s.length == length)
      if (result.length != 1) throw new IllegalArgumentException
      acc + (value -> parts.filter(s => s.length == length)(0))
    }

    def findThree(acc: Map[Int, String]): Map[Int, String] = {
      val candidates = parts
        .filter(s => s.length == 5)
        .filter(s => containsAllChars(s, acc(7)))
      if (candidates.length != 1) throw new IllegalArgumentException

      acc + (3 -> candidates(0))
    }

    def findZero(acc: Map[Int, String]): Map[Int, String] = {
      val dg = subtractString(acc(3), acc(7))
      val candidates = parts
        .filter(s => s.length == 6)
        .filter(s => !containsAllChars(s, dg))
      if (candidates.length != 1) throw new IllegalArgumentException

      acc + (0 -> candidates(0))
    }

    def findSixAndNine(acc: Map[Int, String]): Map[Int, String] = {
      val sixAndNine = parts
        .filter(s => s.length == 6)
        .filter(s => s != acc(0))
      val candidates = sixAndNine
        .filter(s => containsAllChars(s, acc(7)))
      if (candidates.length != 1) throw new IllegalArgumentException
      val nine = candidates(0)
      val six = sixAndNine.filter(s => s != nine)(0)

      acc + (6 -> six, 9 -> nine)
    }

    /*
     * The List of functions allows the use of an accumulator, which
     * is passed into each function and replaced by a new one, containing
     * also the values found by that function. Sort of like recursion,
     * but with all different functions.
     *
     * The idea is to use only immutable objects.
     */
    val functions: List[Map[Int, String] => Map[Int, String]] = List(
      findByLength(1, 2),
      findByLength(4, 4),
      findByLength(7, 3),
      findByLength(8, 7),
      findThree,
      findZero,
      findSixAndNine)

    functions.foldLeft(Map[Int, String]())((acc, func) => func(acc))
  }

  /**
   * Decode the signal patterns, i.e. the left part of the input.
   *
   * The process consists of two steps, parseSignals for step one.
   *
   * Once the representation of each digit is known, the segment ids can be
   * deduced by comparing the digit representations. For example, the difference
   * between the representations of '7' and '1' is segment 'a'.
   *
   * @see [[parseSignals]]
   *
   * @param patterns a String consisting of 10 parts, each representing a digit
   * @return a map of encoded segment -> official segment
   */
  def decodeSignalPatterns(patterns: String): Map[Char, Char] = {
    val digitsLookup = parseSignals(patterns)

    def subtractStrings(acc: Map[Char, Char], segment: Char, first: String, second: String): Map[Char, Char] = {
      val difference = subtractString(first, second)
      if (difference.length != 1) throw new IllegalArgumentException
      acc + (segment -> difference(0))
    }

    def findA(acc: Map[Char, Char]): Map[Char, Char] = {
      subtractStrings(acc, 'a', digitsLookup(7), digitsLookup(1))
    }

    def findD(acc: Map[Char, Char]): Map[Char, Char] = {
      subtractStrings(acc, 'd', digitsLookup(8), digitsLookup(0))
    }

    def findG(acc: Map[Char, Char]): Map[Char, Char] = {
      val segmentG = subtractString(digitsLookup(3), digitsLookup(7)).filter(c => c != acc('d'))
      if (segmentG.length != 1) throw new IllegalArgumentException
      acc + ('g' -> segmentG(0))
    }

    def findB(acc: Map[Char, Char]): Map[Char, Char] = {
      subtractStrings(acc, 'b', digitsLookup(9), digitsLookup(3))
    }

    def findCF(acc: Map[Char, Char]): Map[Char, Char] = {
      val segmentF = digitsLookup(6).intersect(digitsLookup(1))
      val segmentC = subtractString(digitsLookup(1), segmentF)
      acc + ('f' -> segmentF(0), 'c' -> segmentC(0))
    }

    def findE(acc: Map[Char, Char]): Map[Char, Char] = {
      subtractStrings(acc, 'e', digitsLookup(8), digitsLookup(9))
    }

    /*
     * The List of functions allows the use of an accumulator, which
     * is passed into each function and replaced by a new one, containing
     * also the values found by that function. Sort of like recursion,
     * but with all different functions.
     *
     * The idea is to use only immutable objects.
     */
    val functions: List[Map[Char, Char] => Map[Char, Char]] = List(
      findA, findD, findG, findCF, findB, findE)

    /*
     * Execute all the functions.
     *
     * During decoding, the map points from the official segment id to the coded value,
     * which is more useful during decoding.
     */
    val reversed = functions.foldLeft(Map[Char, Char]())((acc, func) => func(acc))
    // Now reverse the keys and the values, so we can use it to decode the input values.
    for ((k, v) <- reversed) yield (v, k)
  }

  def applyTranslation(scrambled: String, translation: Map[Char, Char]): String = {
    scrambled.map(translation).sorted
  }

  /**
   * This uses the translation map created by [[decodeSignalPatterns]] to decode the display value, i.e. the right-hand sde of the input.
   *
   * @param scrambledDisplayValue the rght-hand side of the input
   * @param translation the segment translation table
   * @return the display value
   */
  def rightPartToNumber(scrambledDisplayValue: String, translation: Map[Char, Char]): Int = {
    scrambledDisplayValue
      .split(" ")
      .map(s => s.trim)
      .map(s => s.sorted)
      .map(s => applyTranslation(s, translation))
      .map(sevenSegmentCodes())
      // convert each Int to a String
      .map(i => i.toString)
      // Append them
      .foldLeft("")((acc, s) => acc + s)
      // And the result back to Int
      .toInt
  }

  /**
   * Decode a line to its display value.
   *
   * Create a translation table from the left-hand side, and use that to decode the right-hand side.
   *
   * @param line a line, consisiting of two parts, separated by a '|'
   * @return the display value
   */
  def decodeLine(line: String): Int = {
    val parts = line.split("\\|")

    val signalPatterns = parts(0).trim
    val display = parts(1).trim

    val translation = decodeSignalPatterns(signalPatterns)
    rightPartToNumber(display, translation)
  }

  val lines = readInput(getClass.getResource("input").getFile)

  // Decode the lines and sum the values
  println("Sum of all display values: " + lines.map(decodeLine).sum)
}
