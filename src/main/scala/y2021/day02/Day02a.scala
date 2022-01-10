package y2021.day02

import scala.io.Source

object Day02a extends App {

  case class Coordinates(depth: Int, horizontal: Int)

  def fold(position: Coordinates, input: String) : Coordinates = {

    val upPattern = "up (\\d*)".r
    val downPattern = "down (\\d*)".r
    val forwardPattern = "forward (\\d*)".r

    println(input)

    input match {
      case upPattern(x) => Coordinates(position.depth - x.toInt, position.horizontal)
      case downPattern(x) => Coordinates(position.depth + x.toInt, position.horizontal)
      case forwardPattern(x) => Coordinates(position.depth, position.horizontal + x.toInt)
    }
  }

  val file = getClass.getResource("input").getFile
  val lines = Source.fromFile(file)

  //lines.getLines().foreach(println)

  val position = lines.getLines().foldLeft(Coordinates(0,0))(fold)

  lines.close()

  println("Final position: " + position)
  println("Final multi: " + position.depth * position.horizontal)
}
