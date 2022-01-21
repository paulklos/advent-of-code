package y2021.day18

import y2021.day18.RegularValue.MC_UP

import java.math.{MathContext, RoundingMode}
import scala.language.postfixOps

sealed abstract class NumberLeaf {

  def +(that: NumberLeaf) = NumberNode(this, that)

//  def reduce: NumberLeaf = {
//
//    def iter(acc: NumberLeaf, depth: Int, actionDone: Boolean): (NumberLeaf, Boolean) = {
//      if (actionDone) (acc, actionDone)
//      else {
//        acc match {
//          case p: Pair =>
//            p.explode(depth) match {
//              case (n, true) =>
//                println(f"Exploded pair $p into $n")
//                (n, true)
//              case (n, false) =>
//                println(f"Not exploded pair $p")
//                iter(n.left, depth + 1, actionDone) match {
//                  case (newLeft, newAction) =>
//                    println(f"Left-hand side ${n.left} became $newLeft")
//                    iter(n.right, depth + 1, newAction) match {
//                    case (newRight, actionResult) =>
//                      println(f"Right-hand side ${n.right} became $newRight")
//                      val pair = Pair(newLeft, newRight)
//                      println(f"Returning ($pair, $actionResult)")
//                      (pair, actionResult)
//                  }
//                }
//            }
//
//          case RegularValue(v) if v >= 10 =>
//            val split = RegularValue(v).split
//            println(f"Splitting regular value $v into $split")
//            (split, true)
//          case rv: RegularValue => (rv, actionDone)
//        }
//      }
//    }
//
//    iter(this, 1, actionDone = false) match {
//      case (n, false) =>
//        println(f"No more changes, returning $n")
//        n
//      case (n, true) =>
//        println(f"Changes made, another round with $n")
//        n.reduce
//    }
//  }
}

object NumberLeaf {
  private val DIGIT = "([0-9])"r

  def parse(input: String): NumberLeaf = {

    def iter(acc: String, intermediate: Option[NumberLeaf], pos: Int): NumberLeaf = {
      input.substring(pos, pos + 1) match {
        case "[" => iter("", Option.empty, pos + 1)
        case "," => input.substring(pos + 1, pos + 2) match {
          case "[" =>
            val leaf = RegularValue.parse(acc)
            intermediate match {
              case Some(v) =>
            }
            iter("", Option(leaf), pos + 1)
          case s: String => iter(acc + s, intermediate, pos + 1)
        }
        case "]" => Pair.parse(acc)
      }
    }
    iter("", Option.empty, 0)
  }

}

case class Pair(left: Int, right: Int) extends NumberLeaf {

//  def explode(depth: Int): (Pair, Boolean) = {
//    if (depth < 4) (this, false)
//    else {
//      left match {
//        case Pair(_, r) => right match {
//          case RegularValue(rv) => (Pair(0, rv + r), true)
//          case Pair(_, _) => (Pair(0, 0), true)
//        }
//        case RegularValue(lv) => right match {
//          case Pair(RegularValue(l), RegularValue(_)) => (Pair(lv + l, 0), true)
//          case RegularValue(_) => (this, false)
//        }
//      }
//    }
//  }

  override def toString: String = f"[${left.toString},${right.toString}]"
}

object Pair {
  private val PATTERN = """(\d),(\d)"""r

  def parse(input: String): Pair = {
    input match {
      case PATTERN(l, r) => Pair(l.toInt, r.toInt)
      case _ => throw new IllegalArgumentException(f"Not a Pair: $input")
    }
  }
}
case class RegularValue(value: Int) extends NumberLeaf {

  def split: NumberLeaf = {
    if (value < 10) this
    else Pair(Math.floor(value/2.0).intValue(), BigDecimal(value/2.0, MC_UP).intValue)
  }


  override def toString: String = value.toString
}

object RegularValue {
  private val DIGIT = "([0-9])" r
  private val MC_UP = new MathContext(1, RoundingMode.UP)

  def parse(input: String): NumberLeaf = {

    input.substring(0, 1) match {
      case DIGIT(v) => RegularValue(v.toInt)
      case _ => throw new IllegalArgumentException(f"Not a number: $input")
    }
  }
}

case class NumberNode(left: NumberLeaf, right: NumberLeaf) extends NumberLeaf

object NumberNode {
  def apply(l: Pair, r: Int) = new NumberNode(l, RegularValue(r))
  def apply(l: Int, r: Pair) = new NumberNode(RegularValue(l), r)

}

object Day18a extends App {

}
