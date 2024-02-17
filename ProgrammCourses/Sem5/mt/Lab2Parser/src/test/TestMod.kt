package test

import org.junit.Test
import parser.Parser
import parser.Tree

class TestMod {
    private val parser = Parser()


    @Test
    fun test1() {
        parseExample("var x : Map<Int, Int>;")
    }

    @Test
    fun test2() {
        parseExample("var name : Map<Array<Int>, Int>;")
    }


    private fun parseExample(str : String) : Tree {
        return parser.parse(str.reader())
    }
}