package y2021.day03;

import org.junit.{Assert, Test};

class Day03bTest {

    val testValues: List[String] = List(
        "00100",
        "11110",
        "10110",
        "10111",
        "10101",
        "01111",
        "00111",
        "11100",
        "10000",
        "11001",
        "00010",
        "01010")

    @Test
    def testOxygenGenerator(): Unit = {
        val result = Day03b.find(bitCount => bitCount.moreOnes, '1', '0')(testValues, 0)
        println(result)
        Assert.assertEquals("10111", result)
    }

    @Test
    def testCO2Scrubber(): Unit = {
        val result = Day03b.find(bitCount => bitCount.fewerZeros, '0', '1')(testValues, 0)
        println(result)
        Assert.assertEquals("01010", result)
    }

}
