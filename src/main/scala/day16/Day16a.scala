package day16

import day08.Day08a
import day16.Packet.{binaryToInt}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.matching.Regex

case class Header(version: Int, typeId: Int)

object Header {

  val LENGTH = 6

  def parse(input: String, pointer: Int): Header = {
    Seq(3, 3).foldLeft((Seq[Int](), pointer))((acc, bits) => acc match {
      case (s, p) => (s.appended(binaryToInt(input.substring(p, p + bits))), p + bits)
    }) match {
      case (s, p) =>
        val header = Header(s.head, s(1))
        println(header)
        header
    }
  }

}

abstract class Packet {
  val header: Header
  val subPackets: List[Packet]
  val bits: Int

  def versionSum: Int = {
    header.version + subPackets.foldLeft(0)((acc, sp) => acc + sp.versionSum)
  }
}

object Packet {

  def binaryToInt(in: String): Int = {
    BigInt(in, 2).intValue
  }

  def hexToBinary(hex: String): String = {
    val binaryString = BigInt(hex, 16).toString(2)
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

case class Literal(header: Header, value: Int, bits: Int) extends Packet {
  // Literals don't have sub-packets, so always empty list
  val subPackets: List[Packet] = Nil
}

case class Operator(header: Header, subPackets: List[Packet], bits: Int) extends Packet

object Operator {
  val TYPE_ID_BITS = 1
  val TYPE_ID_ZERO_BITS = 15
  val TYPE_ID_ONE_BITS = 11

  def parse(header: Header, input: String, pointer: Int): Operator = {

    @tailrec
    def readSubPackets(acc: List[Packet], currentPos: Int, bitsToRead: Int): List[Packet] = {
      val packet = Packet.parse(input, currentPos)
      val newAcc = packet :: acc
      val bitsLeft = bitsToRead - packet.bits
      if (bitsLeft == 0) newAcc
      else readSubPackets(newAcc, currentPos + packet.bits, bitsLeft)
    }

    @tailrec
    def readSubPacket(acc: List[Packet], currentPos: Int, packetsToRead: Int): List[Packet] = {
      val packet = Packet.parse(input, currentPos)
      val newAcc = packet :: acc
      val packetsLeft = packetsToRead - 1
      if (packetsLeft == 0) newAcc
      else readSubPacket(newAcc, currentPos + packet.bits, packetsLeft)
    }

    val end = pointer + TYPE_ID_BITS
    val operator = input.substring(pointer, end) match {
      case "0" =>
        val subPacketLength = binaryToInt(input.substring(end, end + TYPE_ID_ZERO_BITS))
        val subPackets = readSubPackets(List(), end + TYPE_ID_ZERO_BITS, subPacketLength)
        Operator(header, subPackets, Header.LENGTH + TYPE_ID_BITS + TYPE_ID_ZERO_BITS + subPacketLength)
      case "1" =>
        val subPacketsToRead = binaryToInt(input.substring(end, end + TYPE_ID_ONE_BITS))
        val subPackets = readSubPacket(List(), end + TYPE_ID_ONE_BITS, subPacketsToRead)
        Operator(header, subPackets, Header.LENGTH + TYPE_ID_BITS + TYPE_ID_ONE_BITS + subPackets.map(sp => sp.bits).sum)
    }
    println("Operator: " + operator)
    println("End pointer: " + operator.bits)
    operator
  }

}

object Literal {
  val MIN_LENGTH = 11
  private val bytePattern: Regex = """1([0-1]{4})"""r
  private val lastBytePattern: Regex = """0([0-1]{4})"""r

  def parse(header: Header, input: String, pointer: Int): Literal = {

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
        val literal = Literal(header, BigInt(binary, 2).intValue, 6 + end - pointer)
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
