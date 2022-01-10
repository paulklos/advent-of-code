package y2021.day14

import y2021.day08.Day08a.readInput
import y2021.day14.Day14a.parseInsertionRules
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14bTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "A polymer" should "be created correctly from a template String" in {
    Given("A template string")
    val template = "NNCB"

    When("A Polymer is created")
    val polymer = PolymerV2.create(template)

    Then("The pairs are correct")
    polymer.pairs shouldEqual Map("NN" -> 1, "NC" -> 1, "CB" -> 1)

    And("The frequencies are correct")
    polymer.freq shouldEqual Map('N' -> 2L, 'B' -> 1L, 'C' -> 1L)
  }

  "A polymer" should "have an extra pair and updated frequencies when a rule is applied" in {
    Given("A template string")
    val template = "AB"

    When("A Polymer is created")
    val polymer = PolymerV2.create(template)

    And("A rule is applied")
    val updated = polymer.replace(Map("AB" -> 'C'))

    Then("The pairs are correct")
    updated.pairs shouldEqual Map("AC" -> 1, "CB" -> 1)

    And("The frequencies are correct for the resulting string 'ACB'")
    updated.freq shouldEqual Map('C' -> 1L, 'A' -> 1L, 'B' -> 1L)
  }

  "The example template" should "have a correct difference between the most and the least frequent character after the rules are applied 40 times" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput").getFile)

    And("containing a polymer template")
    val polymer = PolymerV2.create(lines.head)

    And("parsed into insertion rules")
    val insertionRules = parseInsertionRules(lines)

    When("The rules are applied")
    val result = polymer.replace(insertionRules, times = 40)

    Then("The difference is 2188189693529")
    result.mostFrequentMinusLeastFrequent shouldEqual 2188189693529L
  }

}
