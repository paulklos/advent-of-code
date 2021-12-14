package day03

import scala.io.Source

object Day03a extends App {

  case class BitCount(zero: Int, one: Int) {
    def moreOnes: Boolean = one >= zero
    def fewerZeros: Boolean = zero <= one

    def +(other:BitCount): BitCount = BitCount(zero + other.zero, one + other.one)
  }

  def count(count: List[BitCount], input: String): List[BitCount] = {

    // Count the occurrences of zero and one in a single string.
    // Returns a List of BitCount with an entry for each position
    val single = input.foldLeft(List(): List[BitCount])((a: List[BitCount], c: Char) => {
      c.asDigit match {
        case 0 => a.appended(BitCount(1, 0))
        case 1 => a.appended(BitCount(0, 1))
      }
    })
    println(single)

    count match {
      case Nil =>
        // No accumulator yet, return what we have
        single
      case _ =>
        // Add the count for the current string into the accumulator
        val result = count.zip(single).map(pair => pair._1 + pair._2)
        println(result)
        result
    }
  }

  val file = getClass.getResource("input").getFile
  val lines = Source.fromFile(file)

  val result = lines.getLines().foldLeft(Nil: List[BitCount])(count)

  lines.close()

  println("Final result: " + result)

  val gamma = result.map(b => if (b.one > b.zero) '1' else '0').foldLeft("")((a, b) => a + b)
  val epsilon = result.map(b => if (b.one < b.zero) '1' else '0').foldLeft("")((a, b) => a + b)
  println("Gamma: " + gamma)
  println("Epsilon: " + epsilon)

  private val gammaDec: Int = Integer.parseInt(gamma, 2)
  println("Gamma (Dec): " + gammaDec)
  private val epsilonDec: Int = Integer.parseInt(epsilon, 2)
  println("Epsilon (Dec): " + epsilonDec)
  println("Power consumption: " + gammaDec * epsilonDec)
}
