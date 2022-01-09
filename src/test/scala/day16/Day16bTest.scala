package day16

import day16.Packet.parseHex
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16bTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "The packet C200B40A82" should "have a value of 3" in {
    Given("A packet")
    val input = "C200B40A82"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The value is correct")
    packet match {
      case o: Operator => o.value shouldEqual 3
      case _ => fail("The result must be an operator")
    }
  }

  "The packet 04005AC33890" should "have a value of 54" in {
    Given("A packet")
    val input = "04005AC33890"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The value is correct")
    packet match {
      case o: Operator => o.value shouldEqual 54
      case _ => fail("The result must be an operator")
    }
  }

  "The packet 880086C3E88112" should "have a value of 7" in {
    Given("A packet")
    val input = "880086C3E88112"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The value is correct")
    packet match {
      case o: Operator => o.value shouldEqual 7
      case _ => fail("The result must be an operator")
    }
  }

  "The packet CE00C43D881120" should "have a value of 9" in {
    Given("A packet")
    val input = "CE00C43D881120"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The value is correct")
    packet match {
      case o: Operator => o.value shouldEqual 9
      case _ => fail("The result must be an operator")
    }
  }

  "The packet D8005AC2A8F0" should "have a value of 1" in {
    Given("A packet")
    val input = "D8005AC2A8F0"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The value is correct")
    packet match {
      case o: Operator => o.value shouldEqual 1
      case _ => fail("The result must be an operator")
    }
  }

  "The packet F600BC2D8F" should "have a value of 0" in {
    Given("A packet")
    val input = "F600BC2D8F"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The value is correct")
    packet match {
      case o: Operator => o.value shouldEqual 0
      case _ => fail("The result must be an operator")
    }
  }

  "The packet 9C005AC2F8F0" should "have a value of 0" in {
    Given("A packet")
    val input = "9C005AC2F8F0"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The value is correct")
    packet match {
      case o: Operator => o.value shouldEqual 0
      case _ => fail("The result must be an operator")
    }
  }

  "The packet 9C0141080250320F1802104A08" should "have a value of 1" in {
    Given("A packet")
    val input = "9C0141080250320F1802104A08"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The value is correct")
    packet match {
      case o: Operator => o.value shouldEqual 1
      case _ => fail("The result must be an operator")
    }
  }

}
