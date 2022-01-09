package day16

import day16.Packet.parseHex
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16aTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "A literal packet" should "be parsed correctly" in {
    Given("A packet")
    val packet = "D2FE28"

    When("The input is parsed")
    val literal = parseHex(packet)

    Then("The literal is correct")
    literal shouldEqual Literal(Header(6, 4), 2021, 21)
  }

  "An operator packet with type id '0'" should "be parsed correctly" in {
    Given("A packet")
    val packet = "38006F45291200"

    When("The input is parsed")
    val operator = parseHex(packet)

    Then("The literal is correct")
    operator shouldEqual Operator(Header(1, 6), List(Literal(Header(6,4), 10, 11), Literal(Header(2,4), 20, 16)), 49)
  }

  "An operator packet with type id '1'" should "be parsed correctly" in {
    Given("A packet")
    val packet = "EE00D40C823060"

    When("The input is parsed")
    val operator = parseHex(packet)

    Then("The literal is correct")
    // USe shouldEqual because the order must match for the operators to work properly
    operator shouldEqual Operator(Header(7, 3), List(Literal(Header(2,4), 1, 11), Literal(Header(4,4), 2, 11), Literal(Header(1,4), 3, 11)), 51)
  }

  "The packet '8A004A801A8002F478'" should "have a version sum of 16" in {
    Given("A packet")
    val input = "8A004A801A8002F478"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The version sum is 16")
    packet.versionSum shouldEqual 16
  }

  "The packet '620080001611562C8802118E34'" should "have a version sum of 12" in {
    Given("A packet")
    val input = "620080001611562C8802118E34"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The version sum is 12")
    packet.versionSum shouldEqual 12
  }

  "The packet 'C0015000016115A2E0802F182340'" should "have a version sum of 23" in {
    Given("A packet")
    val input = "C0015000016115A2E0802F182340"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The version sum is 23")
    packet.versionSum shouldEqual 23
  }

  "The packet 'A0016C880162017C3686B18A3D4780'" should "have a version sum of 31" in {
    Given("A packet")
    val input = "A0016C880162017C3686B18A3D4780"

    When("The input is parsed")
    val packet = parseHex(input)

    Then("The version sum is 31")
    packet.versionSum shouldEqual 31
  }

}
