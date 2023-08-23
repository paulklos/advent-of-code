package y2021.day18

import scala.annotation.tailrec
import scala.language.postfixOps

abstract class Value {
  def incDepth: Value
  def +(that: Value) = Pair(this.incDepth, that.incDepth, 0)
}

case class Regular(value: Int) extends Value {
  override def incDepth: Value = this
  override def toString: String = value.toString
}
case class Pair(left: Value, right: Value, depth: Int) extends Value {
  override def incDepth: Value = Pair(left.incDepth, right.incDepth, depth + 1)

  def reduce: Pair = {
    val newLeft = left match {
      case r: Regular => r
      case Pair()
    }
    this
  }
  override def toString: String = f"[$left,$right]"
}

object Pair {
  def apply(l: Int, r: Int, depth: Int) = new Pair(Regular(l), Regular(r), depth)
  def apply(l: Int, r: Value, depth: Int) = new Pair(Regular(l), r, depth)
  def apply(l: Value, r: Int, depth: Int) = new Pair(l, Regular(r), depth)

  def parse(input: String): Pair = {

    @tailrec
    def iter(values: List[Value], pos: Int, depth: Int): Pair = {
      if (pos == input.length) {
        values match {
          case p :: Nil => p match {
            case p: Pair => p
          }
          case _ => throw new IllegalArgumentException("Not a Pair")
        }
      } else {
        val character = input.substring(pos, pos + 1)
        character match {
          case "[" => iter(values, pos + 1, depth + 1)
          case "," =>
            iter(values, pos + 1, depth)
          case "]" =>
            values match {
              case r :: l :: xs =>
                val pair = Pair(l, r, depth - 1)
                iter(pair :: xs, pos + 1, depth - 1)
              case _ => throw new IllegalArgumentException("Two Pairs needed at a ']'")
            }

          case s: String => iter(Regular(s.toInt) :: values, pos + 1, depth)
        }
      }
    }
    iter(List(), 0, 0)
  }

}

object Day18a extends App {

}
