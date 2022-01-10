package y2021.day16

import y2021.day08.Day08a

object Day16b extends App {

  val input = Day08a.readInput(getClass.getResource("input").getFile).head

  val packet = Packet.parseHex(input)
  println("The packet value is: " + packet.value)

}
