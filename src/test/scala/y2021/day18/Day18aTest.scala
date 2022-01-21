package y2021.day18

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import y2021.day08.Day08a

class Day18aTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "Adding two Snailfish numbers" should "result in a new SnailFishNumber" in {
    Given("A left operand")
    val l = Pair(1, 2)

    And("a right operand")
    val r = NumberNode(Pair(3, 4), RegularValue(5))

    When("The values are added")
    val result = l + r

    Then("The result is the Pair [[1,2],[3,4],5]")
    result shouldEqual NumberNode(Pair(1, 2), NumberNode(Pair(3, 4), 5))
  }

  "The Snailfish number string [9,8]" can "be parsed into a SnailFishNumber" in {
    Given("A number string")
    val input = "[9,8]"

    When("The number is parsed")
    val sfn = NumberNode.parse(input)

    Then("The number is correct")
    sfn shouldEqual Pair(9, 8)
  }

//  "The Snailfish number string [9,[8,3]]" can "be parsed into a SnailFishNumber" in {
//    Given("A number string")
//    val input = "[9,[8,3]]"
//
//    When("The number is parsed")
//    val sfn = NumberLeaf.parse(input)
//
//    Then("The number is correct")
//    sfn shouldEqual Pair(9, Pair(8, 3))
//  }
//
//  "The Snailfish number string [[[[[9,8],1],2],3],4]" can "be parsed into a SnailFishNumber" in {
//    Given("A number string")
//    val input = "[[[[[9,8],1],2],3],4]"
//
//    When("The number is parsed")
//    val sfn = NumberLeaf.parse(input)
//
//    Then("The number is correct")
//    sfn shouldEqual Pair(Pair(Pair(Pair(Pair(9, 8), 1), 2), 3), 4)
//  }
//
//  "The input file" must "be read and parsed correctly" in {
//    Given("The input file")
//    val file = getClass.getResource("input").getFile
//
//    When("The file is read")
//    val input = Day08a.readInput(file)
//
//    Then("Parsing the line into a Snailfish number and outputting that as a string result in the line")
//    input.foreach(s => {
//      val sfn = NumberLeaf.parse(s)
//      println(sfn)
//      sfn.toString shouldEqual s
//    })
//  }
//
//  "A left pair" can "be exploded" in {
//    Given("A pair containing one other pair")
//    val pair = Pair(Pair(2,3),6)
//
//    When("The pair is exploded")
//    val exploded = pair.explode(4)
//
//    Then("The result is correct")
//    exploded shouldEqual (Pair(0,9), true)
//  }
//
//  "A right pair" can "be exploded" in {
//    Given("A pair containing one other pair")
//    val pair = Pair(6, Pair(2,3))
//
//    When("The pair is exploded")
//    val exploded = pair.explode(4)
//
//    Then("The result is correct")
//    exploded shouldEqual (Pair(8,0), true)
//  }
//
//  "A pair consisting of two pairs" must "explode to (0,0)" in {
//    Given("A pair containing one other pair")
//    val pair = Pair(Pair(6,8), Pair(2,3))
//
//    When("The pair is exploded")
//    val exploded = pair.explode(4)
//
//    Then("The result is correct")
//    exploded shouldEqual (Pair(0,0), true)
//  }
//
//  "A pair consisting of regular values" must "explode to itself" in {
//    Given("A pair containing one other pair")
//    val pair = Pair(6,8)
//
//    When("The pair is exploded")
//    val exploded = pair.explode(4)
//
//    Then("The result is correct")
//    exploded shouldEqual (pair, false)
//  }
//
//  "A regular number smaller than 10" must "remain unchanged when split" in {
//    Given("A regular value smaller than 10")
//    val number = RegularValue(8)
//
//    When("The value is split")
//    val split = number.split
//
//    Then("The result is the same value")
//    split shouldEqual number
//  }
//
//  "An even regular value" must "be split in a pair with two equal values" in {
//    Given("A regular value smaller than 10")
//    val number = RegularValue(14)
//
//    When("The value is split")
//    val split = number.split
//
//    Then("The result is the same value")
//    split shouldEqual Pair(7,7)
//  }
//
//  "An odd regular value" must "be split into a pair where the left value is rounded down and the right value is rounded up" in {
//    Given("A regular value smaller than 10")
//    val number = RegularValue(13)
//
//    When("The value is split")
//    val split = number.split
//
//    Then("The result is the same value")
//    split shouldEqual Pair(6,7)
//  }
//
//  "A regular number greater than 10" must "be split" in {
//    Given("A pair containing one other pair")
//    val pair = Pair(6, Pair(2,3))
//
//    When("The pair is exploded")
//    val exploded = pair.explode(4)
//
//    Then("The result is correct")
//    exploded shouldEqual (Pair(8,0), true)
//  }
//
//  "The number [[[[[9,8],1],2],3],4]" must "be reduced to [[[[0,9],2],3],4]" in {
//    Given("A snailfish number")
//    val pair = Pair(Pair(Pair(Pair(Pair(9,8),1),2),3),4)
//
//    When("The number is reduced")
//    val exploded = pair.reduce
//
//    Then("The result is correct")
//    exploded shouldEqual Pair(Pair(Pair(Pair(0,9),2),3),4)
//
//  }
//
//  "The number [[[[0,7],4],[15,[0,7]]],[1,1]]" must "be reduced to [[[[0,7],4],[[7,8],[0,7]]],[1,1]]" in {
//    Given("A snailfish number")
//    val pair = Pair(Pair(Pair(Pair(0,7),4),Pair(15,Pair(0,7))),Pair(1,1))
//
//    When("The number is reduced")
//    val exploded = pair.reduce
//
//    Then("The result is correct")
//    exploded shouldEqual Pair(Pair(Pair(Pair(0,7),4),Pair(Pair(7,8),Pair(0,7))),Pair(1,1))
//
//  }
//
//  "The reduced result of [[[[4,3],4],4],[7,[[8,4],9]]] + [1,1]" must "be [[[[0,7],4],[[7,8],[6,0]]],[8,1]]" in {
//    Given("A snailfish number")
//    val pair = Pair(Pair(Pair(Pair(4,3),4),4),Pair(7,Pair(Pair(8,4),9))) + Pair(1,1)
//    println("sum: " + pair)
//
//    When("The number is reduced")
//    val exploded = pair.reduce
//
//    Then("The result is correct")
//    exploded shouldEqual Pair(Pair(Pair(Pair(0,7),4),Pair(Pair(7,8),Pair(6,0))),Pair(8,1))
//
//  }
//
}
