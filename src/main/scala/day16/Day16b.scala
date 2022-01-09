package day16

import day08.Day08a

object Day16b extends App {

  val input = Day08a.readInput(getClass.getResource("input").getFile).head

  val packet = Packet.parseHex(input)
  println("The packet value is: " + packet.value)

}
