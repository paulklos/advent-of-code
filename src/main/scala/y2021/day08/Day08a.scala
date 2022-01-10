package y2021.day08

import scala.io.Source
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
 * er is 1 waarde van 6 tekens waar de 1 niet in zit. Dat is de 6
 *
 * er is 1 waarde van 5 tekens waar de 7 in zit. Dat is de 3
 * 3 - 7 => dg
 * Er is 1 waarde van 6 tekens waar dg niet in zit, dat is de 0
 *
 * Het verschil tussen 0 en 8 is segment d
 *
 * Dus daarmee is g ook bekend
 *
 * Bij de 6 en de 9 zit er 1 die ook 7 bevat, dat is de 9
 * Daarmee is segment b bekend
 *
 * Uit de doorsnede tussen 6 en 1 kunnen c en f bepaald worden
 *
 * Het laatste segemnt is dan e
 *
 * Dus nu weten we ook welke de 2 is en welke de 5
 */
object Day08a extends App {

  def splitRight(lines: List[String])(f: String => Boolean): Int = {
    lines
      .map(line => line.split("\\|"))
      .filter(parts => parts.length == 2)
      .map(parts => parts(1))
      .map(part => part.trim)
      .flatMap(part => part.split(" ").toList)
      .count(f)
  }

  def readInput(file: String): List[String] = {
    val input = Source.fromFile(file)

    val lines = input.getLines().toList

    input.close()

    lines
  }

  def easyNumbers(code: String): Boolean = {
    code.length == 2 || code.length == 3 || code.length == 4 || code.length == 7
  }

  val lines = readInput(getClass.getResource("input").getFile)

  println(lines)
  println(splitRight(lines)(easyNumbers))
}
