package day16

import day08.Day08a
import day16.Operator.{EQ, GT, LT, MAX, MIN, PRODUCT, SUM}
import day16.Packet.binaryToInt

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.matching.Regex

case class Header(version: Int, typeId: Int)

object Header {

  val LENGTH = 6
  val FIELD_LENGTH = 6

  /**
   * Read two sets of three bits, representing the version and the type id.
   * @param input the input string
   * @param pointer the parsing position
   * @return a Header
   */
  def parse(input: String, pointer: Int): Header = {
    val header = Header(
      binaryToInt(input.substring(pointer, pointer + FIELD_LENGTH)),
      binaryToInt(input.substring(pointer + FIELD_LENGTH, pointer + LENGTH)))
    println(header)
    header
  }

}

abstract class Packet {
  val header: Header
  val subPackets: List[Packet]
  val bits: Int

  /**
   * @return The sum of all versions in the packet
   */
  def versionSum: Int = {
    header.version + subPackets.foldLeft(0)((acc, sp) => acc + sp.versionSum)
  }

  // The packet value
  def value: Long
}

object Packet {

  def binaryToInt(in: String): Int = {
    BigInt(in, 2).intValue
  }

  def hexToBinary(hex: String): String = {
    val binaryString = BigInt(hex, 16).toString(2)
    // Scala leaves out any leading zeros, but we need them
    ("0" * (4 * hex.length - binaryString.length)) + binaryString
  }

  def parseHex(input: String): Packet = {
    val binaryString = hexToBinary(input)
    println(binaryString)
    parse(binaryString)
  }

  def parse(binaryString: String, pointer: Int = 0): Packet = {

    val header = Header.parse(binaryString, pointer)
    header match {
      case Header(_, 4) =>
        Literal.parse(header, binaryString, pointer + Header.LENGTH)
      case Header(_, _) =>
        Operator.parse(header, binaryString, pointer + Header.LENGTH)
    }
  }

}

/**
 * A packet representing a literal value.
 *
 * @param header the header
 * @param value implements [[Packet.value]]
 * @param bits the number of bits in the input string
 */
case class Literal(header: Header, value: Long, bits: Int) extends Packet {
  // Literals don't have sub-packets
  val subPackets: List[Packet] = Nil
}

/**
 * A packet representing an operator..
 *
 * @param header the header
 * @param subPackets the sub-packets
 * @param bits the number of bits in the input string. This includes the sub-packet bits
 */
case class Operator(header: Header, subPackets: List[Packet], bits: Int) extends Packet {
  /**
   * Implements [[Packet.value]].
   *
   * @return the value, calculated according to the header's type id
   */
  def value: Long = {
    header.typeId match {
      case SUM => subPackets.foldLeft(0L)((acc, p) => acc + p.value)
      case PRODUCT => subPackets.foldLeft(1L)((acc, p) => acc * p.value)
      case MIN => subPackets.map(sp => sp.value).min
      case MAX => subPackets.map(sp => sp.value).max
      case GT => subPackets match {
        case x::y::_ => if (x.value > y.value) 1 else 0
      }
      case LT => subPackets match {
        case x::y::_ => if (x.value < y.value) 1 else 0
      }
      case EQ => subPackets match {
        case x::y::_ => if (x.value == y.value) 1 else 0
      }
    }
  }
}

object Operator {
  val TYPE_ID_BITS = 1
  val TYPE_ID_ZERO_BITS = 15
  val TYPE_ID_ONE_BITS = 11

  // Operations
  val SUM = 0
  val PRODUCT = 1
  val MIN = 2
  val MAX = 3
  val GT = 5
  val LT = 6
  val EQ = 7

  /**
   * Parse an operator from the input string, starting at the given position.
   *
   * @param header the resulting Operator's header
   * @param input the input stgring
   * @param pointer the parsing pointer
   * @return the Operator
   */
  def parse(header: Header, input: String, pointer: Int): Operator = {

    /**
     * Parse a sub-packet and add it to the list.
     *
     * @param acc th accumulator
     * @param currentPos the current position in the input string
     * @param countdown when the countdown reaches zero, parsing is done
     * @param next a function to determine the countdown value for the next iteration
     * @return a list of sub-packets
     */
    @tailrec
    def readSubPacket(acc: List[Packet], currentPos: Int, countdown: Int)(next: (Packet, Int) => Int): List[Packet] = {
      val packet = Packet.parse(input, currentPos)
      val newAcc = packet :: acc
      next(packet, countdown) match {
        // Reverse the accumulator because the sub-packets have been prepended
        case 0 => newAcc.reverse
        case toDo => readSubPacket(newAcc, currentPos + packet.bits, toDo)(next)
      }
    }

    val end = pointer + TYPE_ID_BITS
    val operator = input.substring(pointer, end) match {
      case "0" =>
        // The length is defined as a number of bits
        // The function is to subtract the number of bits of the packet that was just parsed from the countdown
        val subPacketLength = binaryToInt(input.substring(end, end + TYPE_ID_ZERO_BITS))
        val subPackets = readSubPacket(List(), end + TYPE_ID_ZERO_BITS, subPacketLength)((p, bitsLeft) => bitsLeft - p.bits)
        Operator(header, subPackets, Header.LENGTH + TYPE_ID_BITS + TYPE_ID_ZERO_BITS + subPacketLength)
      case "1" =>
        // The length is defined as a number sub-packets
        // The function is simply to subtract one from the countdown, since the packet size doesn't matter
        val subPacketsToRead = binaryToInt(input.substring(end, end + TYPE_ID_ONE_BITS))
        val subPackets = readSubPacket(List(), end + TYPE_ID_ONE_BITS, subPacketsToRead)((_, iterations) => iterations - 1)
        Operator(header, subPackets, Header.LENGTH + TYPE_ID_BITS + TYPE_ID_ONE_BITS + subPackets.map(sp => sp.bits).sum)
    }
    println("Operator: " + operator)
    println("End pointer: " + operator.bits)
    operator
  }

}

object Literal {
  val MIN_LENGTH = 11
  // Pattern representing four bits that are followed by more bits
  private val bytePattern: Regex = """1([0-1]{4})"""r
  // Pattern representing the last four bits of the value
  private val lastBytePattern: Regex = """0([0-1]{4})"""r

  /**
   * Parse a literal from the input string, starting at the given position.
   *
   * @param header the resulting Literal's header
   * @param input the input string
   * @param pointer the parsing position
   * @return the Literal
   */
  def parse(header: Header, input: String, pointer: Int): Literal = {

    /**
     * Collect the bits of the value, in groups of four.
     *
     * @param acc the binary string being built
     * @param pointer the parsing pointer
     * @return a pair containing the binary string and the total number of bits read
     *         each group of four bits are five bits read because of the prefix
     */
    @tailrec
    def parseByte(acc: String, pointer: Int): (String, Int) = {
      val endPos = pointer + 5
      val part = input.substring(pointer, endPos)
      part match {
        case lastBytePattern(byte) => (acc + byte, endPos)
        case bytePattern(byte) => parseByte(acc + byte, endPos)
      }
    }

    parseByte("", pointer) match {
      case (binary, end) =>
        val literal = Literal(header, BigInt(binary, 2).longValue, Header.LENGTH + end - pointer)
        println("Literal value: " + literal)
        println("End pointer: " + literal.bits)
        literal
    }
  }

}

object Day16a extends App {

  val input = Day08a.readInput(getClass.getResource("input").getFile).head

  val packet = Packet.parseHex(input)
  println("The version sum is: " + packet.versionSum)

}
