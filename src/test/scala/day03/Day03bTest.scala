package day03;

import junit.framework.TestCase;
import org.junit.Assert;
import org.junit.Test;
import scala.Unit;

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
    def testOxygenGenerator() {
        val result = Day03b.find(bitCount => bitCount.moreOnes, '1', '0')(testValues, 0)
        println(result)
        Assert.assertEquals("10111", result)
    }

    @Test
    def testCO2Scrubber() {
        val result = Day03b.find(bitCount => bitCount.moreZeros, '0', '1')(testValues, 0)
        println(result)
        Assert.assertEquals("01010", result)
    }

}
