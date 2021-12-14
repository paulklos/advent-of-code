package day02

import scala.io.Source

object Day02b extends App {

  case class Coordinates(depth: Int, horizontal: Int, aim: Int)

  def fold(current: Coordinates, input: String) : Coordinates = {

    val upPattern = "up (\\d*)".r
    val downPattern = "down (\\d*)".r
    val forwardPattern = "forward (\\d*)".r

    println(input)

    input match {
      case upPattern(x) => Coordinates(current.depth, current.horizontal, current.aim - x.toInt)
      case downPattern(x) => Coordinates(current.depth, current.horizontal, current.aim + x.toInt)
      case forwardPattern(x) => Coordinates(current.depth + x.toInt * (current.aim), current.horizontal + x.toInt, current.aim)
    }
  }

  val file = getClass.getResource("input").getFile
  val lines = Source.fromFile(file)

  //lines.getLines().foreach(println)

  val position = lines.getLines().foldLeft(Coordinates(0,0,0))(fold)

  lines.close()

  println("Final position: " + position)
  println("Final multi: " + position.depth * position.horizontal)
}
