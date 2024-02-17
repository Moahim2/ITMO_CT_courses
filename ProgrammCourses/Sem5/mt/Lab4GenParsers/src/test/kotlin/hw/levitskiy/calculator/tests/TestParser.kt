package hw.levitskiy.calculator.tests

import hw.levitskiy.`$generating$`.calculator.Parser
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.fail
import java.text.ParseException


class TestParser {
    private val parser = Parser()

    @Test
    fun simpleTests() {
        simpleTest("1")
        simpleTest("1 + 1")
        simpleTest("1 - 1")
        simpleTest("11231231 * 10000")
        simpleTest("0 / 1")

        simpleTest("1 + 2 + 3 + 4 - 5 - 6")
        simpleTest("1 + 2 + 3 + 4 - 5 * 6")

        simpleTest("(1)")
        simpleTest("1 + 2")
        simpleTest("2 * (1 - 1)")
        simpleTest("(10 - 7)/3")

        simpleTest("(4 * 1 + (2) * 3) + ((1 - 7) / 5 + (1 + 2 + (3 * 3 + 7) * 11 - 1 - (1 - (2 * 3 - (1 + (11 - 10000))))))")


        //modification:
        simpleTest("1!")
        simpleTest("2!!")
        simpleTest("(2 + 2)!!")
        simpleTest("(2 + 2)! + 5! + 7!")
    }


    @Test
    fun simpleErrorTests() {
        simpleErrorTest("")
        simpleErrorTest("()")
        simpleErrorTest("((1)")
        simpleErrorTest("1 +")
        simpleErrorTest("+ 2")
        simpleErrorTest("1 + 2 )")

        simpleErrorTest("-")

        simpleErrorTest("1 + 2 + 4 5")
        simpleErrorTest("1 + 2 + 3 + (4 + ")
        simpleErrorTest("(1 * 2 + 5) - 3 ))))))))))))))")
    }

    //Test attributes.
    @Test
    fun simpleCalcTests() {
        simpleCalcTest("1", 1)
        simpleCalcTest("1 + 1", 2)
        simpleCalcTest("1 + 2 * 3", 7)
        simpleCalcTest("(1 + 2) * 3 / 3 + 7 + 5 * (2 + 3)", 35)
        simpleCalcTest("1 - 1 - 1", -1)

        //modification
        simpleCalcTest("1!", 1)
        simpleCalcTest("2!!", 2)
        simpleCalcTest("(1 + 1)!!", 2)
        simpleCalcTest("(2 + 2)! + 2! + 2!", 28)

        simpleCalcTest("6!", 720)

        simpleCalcTest("((2!)!!!!!!!)!!!!!!!", 2)

        simpleCalcTest("(2! * 3! * 4! / 4!)!", 479001600)
    }

    private fun simpleErrorTest(str: String) {
        try {
            simpleTest(str)
            fail("Must be error, because input isn't correct")
        } catch (e : ParseException) {
            println(e.message)
        }
    }

    private fun simpleTest(str : String) {
        parser.parse(str.reader())
    }

    private fun simpleCalcTest(str : String, res : Int) {
        assertEquals(res, parser.parse(str.reader()).first)
    }


}
