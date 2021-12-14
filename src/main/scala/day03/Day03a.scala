package day03

import scala.io.Source

object Day03a extends App {

  case class BitCount(zero: Int, one: Int)

  def count(count: List[BitCount], input: String): List[BitCount] = {

    val single = input.foldLeft(List(): List[BitCount])((a: List[BitCount], c: Char) => {
      c.asDigit match {
        case 0 => a.appended(BitCount(1, 0))
        case 1 => a.appended(BitCount(0, 1))
      }
    })
    println(single)

    count match {
      case Nil => {
        println("1")
        single
      }
      case _ => {
        println("2")
        val result = count.zip(single).map(pair => BitCount(pair._1.zero + pair._2.zero, pair._1.one + pair._2.one))
        println(result)
        result
      }
    }
  }


  val file = getClass.getResource("input").getFile
  val lines = Source.fromFile(file)

  val result = lines.getLines().foldLeft(Nil: List[BitCount])(count)

  lines.close()

  println("Final result: " + result)
//  println("Final multi: " + position.depth * position.horizontal)

  val gamma = result.map(b => if (b.one > b.zero) '1' else '0').foldLeft("")((a, b) => a + b)
  val epsilon = result.map(b => if (b.one < b.zero) '1' else '0').foldLeft("")((a, b) => a + b)
  println(gamma)
  private val gammaDec: Int = Integer.parseInt(gamma, 2)
  println(gammaDec)
  println(epsilon)
  private val epsilonDec: Int = Integer.parseInt(epsilon, 2)
  println(epsilonDec)
  println(gammaDec * epsilonDec)
}
