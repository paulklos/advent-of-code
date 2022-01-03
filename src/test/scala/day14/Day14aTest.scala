package day14

import day08.Day08a.readInput
import day14.Day14a.parseInsertionRules
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14aTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "A list of insertion rules" should "be read into a map" in {
    Given("Input lines")
    val input = List(
      "",
      "CH -> B",
      "HH -> N",
      "CB -> H"
    )

    When("The insertion rules are parsed")
    val ruleMap = parseInsertionRules(input)

    Then("The map is correct")
    ruleMap.size shouldEqual 3
  }

  "A polymer" should "apply an insertion correctly" in {
    Given("A polymer")
    val polymer = Polymer("AA")

    And("insertion rules")
    val rules = Map("AA" -> 'C')

    When("The rules are applied")
    val result = polymer.replace(rules)

    Then("The replacement character is inserted")
    result.sequence shouldBe "ACA"
  }

  "A polymer" should "stay the same if no rules apply" in {
    Given("A polymer")
    val polymer = Polymer("AA")

    And("insertion rules")
    val rules = Map("AB" -> 'C')

    When("The rules are applied")
    val result = polymer.replace(rules)

    Then("The replacement character is inserted")
    result.sequence shouldBe "AA"
  }

  "A polymer" should "stay apply all the applicable rules " in {
    Given("A polymer")
    val polymer = Polymer("ABA")

    And("insertion rules")
    val rules = Map("AB" -> 'C', "BA" -> 'D', "EF" -> 'G')

    When("The rules are applied")
    val result = polymer.replace(rules)

    Then("The replacement characters are inserted")
    result.sequence shouldBe "ACBDA"
  }

  "The example template" should "apply the rules correctly" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput").getFile)

    And("containing a polymer template")
    val polymer = Polymer(lines.head)

    And("parsed into insertion rules")
    val insertionRules = parseInsertionRules(lines)

    When("The rules are applied")
    val result = polymer.replace(insertionRules)

    Then("The result is correct")
    result.sequence shouldBe "NCNBCHB"
  }

  "The example step 2" should "apply the rules correctly" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput").getFile)

    And("parsed into insertion rules")
    val insertionRules = parseInsertionRules(lines)

    And("A polymer template")
    val polymer = Polymer("NCNBCHB")

    When("The rules are applied")
    val result = polymer.replace(insertionRules)

    Then("The result is correct")
    result.sequence shouldBe "NBCCNBBBCBHCB"
  }

  "The example step 3" should "apply the rules correctly" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput").getFile)

    And("parsed into insertion rules")
    val insertionRules = parseInsertionRules(lines)

    And("A polymer template")
    val polymer = Polymer("NBCCNBBBCBHCB")

    When("The rules are applied")
    val result = polymer.replace(insertionRules)

    Then("The result is correct")
    result.sequence shouldBe "NBBBCNCCNBBNBNBBCHBHHBCHB"
  }

  "The example step 4" should "apply the rules correctly" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput").getFile)

    And("parsed into insertion rules")
    val insertionRules = parseInsertionRules(lines)

    And("A polymer template")
    val polymer = Polymer("NBBBCNCCNBBNBNBBCHBHHBCHB")

    When("The rules are applied")
    val result = polymer.replace(insertionRules)

    Then("The result is correct")
    result.sequence shouldBe "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
  }

  "The example template" should "apply the rules correctly 10 times" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput").getFile)

    And("containing a polymer template")
    val polymer = Polymer(lines.head)

    And("parsed into insertion rules")
    val insertionRules = parseInsertionRules(lines)

    When("The rules are applied")
    val result = polymer.replace(insertionRules, 10)

    Then("The result has a length of 3073")
    result.sequence.length shouldEqual 3073

    And("B occurs 1749 times")
    val frequencies = result.frequencies
    frequencies('B') shouldEqual 1749

    And("C occurs 289 times")
    frequencies('C') shouldEqual 298
  }

  "The example template" should "have a correct difference between the most and the least frequent character after the rules are applied 10 times" in {
    Given("An example input file")
    val lines = readInput(getClass.getResource("testInput").getFile)

    And("containing a polymer template")
    val polymer = Polymer(lines.head)

    And("parsed into insertion rules")
    val insertionRules = parseInsertionRules(lines)

    When("The rules are applied")
    val result = polymer.replace(insertionRules, 10)

    Then("The difference is 1588")
    result.mostFrequentMinusLeastFrequent shouldEqual 1588
  }

}
