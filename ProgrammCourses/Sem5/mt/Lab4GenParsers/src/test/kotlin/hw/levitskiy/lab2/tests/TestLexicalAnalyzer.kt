package hw.levitskiy.lab2.tests

import hw.levitskiy.`$generating$`.lab2.Lexer
import hw.levitskiy.`$generating$`.lab2.Terminal
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.fail
import java.text.ParseException
import java.util.*
import kotlin.collections.ArrayList

class TestLexicalAnalyzer {
    private val random = Random(4875043285743285204L)
    private val correctWhitespaces = listOf("\t", "\r", "\n", " ")
    private val correctTerminals = mapOf(
        Pair("name", Terminal.NAME),
        Pair("Array", Terminal.ARR),
        Pair("var", Terminal.VAR),
        Pair("<", Terminal.LBR),
        Pair(">", Terminal.RBR),
        Pair(":", Terminal.COL),
        Pair(",", Terminal.COM),
        Pair(";", Terminal.SCOL),
    )


    /**
     * Проверка всех токенов по отдельности.
     */
    @Test
    fun test1CorrectFullTerminals() {
        correctTerminals.forEach {
            val str = if (it.key != "$") {
                it.key
            } else {
                (-1).toChar().toString()
            }
            correctTerminals(str, it.value)
        }
    }

    /**
     * Проверка некоторых токенов вместе с корректными пробельными символами.
     */
    @Test
    fun test2CorrectWhitespaces() {
        correctTerminals(" ")
        correctTerminals("      var     ", Terminal.VAR)
        correctTerminals("\tarray         \r\n", Terminal.NAME)
        correctTerminals("\n \r \t \n \r         :         \t \n \r", Terminal.COL)
    }

    /**
     * Проверка рандомных последовательностей корректных пробельных символов с одним токеном.
     */
    @Test
    fun test3RandomCorrectWhitespaces() {
        repeat(10) {
            var str = ""
            repeat(1000) { it1 ->
                str += correctWhitespaces[random.nextInt(0, 4)]
                if (it1 == 500) {
                    str += Terminal.NAME.name
                }
            }
            correctTerminals(str, Terminal.NAME)
        }
    }

    /**
     * Проверка совокупности корректных токенов и пробельных символов
     */
    @Test
    fun test4CorrectTerminalList() {
        correctTerminals(
            "var x : Array<Int>;", Terminal.VAR, Terminal.NAME, Terminal.COL, Terminal.ARR,
            Terminal.LBR, Terminal.NAME, Terminal.RBR, Terminal.SCOL, Terminal.`$`
        )
        correctTerminals(
            "var var var Array var :::,,,,             Int,          \tName,;;;;<<<           >>>",
            Terminal.VAR, Terminal.VAR, Terminal.VAR, Terminal.ARR, Terminal.VAR, Terminal.COL, Terminal.COL, Terminal.COL,
            Terminal.COM, Terminal.COM, Terminal.COM, Terminal.COM, Terminal.NAME, Terminal.COM, Terminal.NAME, Terminal.COM,
            Terminal.SCOL, Terminal.SCOL, Terminal.SCOL, Terminal.SCOL, Terminal.LBR, Terminal.LBR, Terminal.LBR, Terminal.RBR,
            Terminal.RBR, Terminal.RBR, Terminal.`$`
        )
    }

    /**
     * Проверка рандомных последовательностей из корректных токенов.
     */
    @Test
    fun test5RandomCorrectTerminalList() {
        repeat(10) {
            val (str, terminalsList) = generateRandomCorrectTask()
            correctTerminals(str, *terminalsList.toTypedArray())
        }
    }

    /**
     * Проверка на непринятие некорректных токенов
     */
    @Test
    fun test6IncorrectTerminals() {
        hasIncorrectTerminal("\'", '\'')
        hasIncorrectTerminal("var x : Array<11231231231231Int>;", '1')
        hasIncorrectTerminal("var ---------- x : Array<Int>;", '-')
    }


    /**
     * Рандомная проверка на непринятие некорректных токенов
     */
    @Test
    fun test7RandomIncorrectTerminals() {
        repeat(10) {
            val (str1, _) = generateRandomCorrectTask()
            var ch : Char
            while (true) {
                Char.MAX_VALUE
                ch = random.nextInt(Char.MIN_VALUE.code, Char.MAX_VALUE.code).toChar()
                if (ch != (-1).toChar() && !ch.isLetter() &&
                    ch != ':' && ch != ',' && ch != ';' && ch != '<' && ch != '>') {
                    break
                }
            }
            val (str2, _) = generateRandomCorrectTask()
            hasIncorrectTerminal(str1 + ch + str2, ch)
        }
    }





    private fun generateRandomCorrectTask() : Pair<String, List<Terminal>> {
        val terminalsList = ArrayList<Terminal>()
        var str = ""
        repeat(random.nextInt(1, 1000)) {
            val t = correctTerminals.toList()[random.nextInt(0, 7)]
            val curTerminal = t.second
            terminalsList.add(curTerminal)
            str += t.first
            if (curTerminal == Terminal.VAR || curTerminal == Terminal.NAME || curTerminal == Terminal.ARR) {
                str += correctWhitespaces[random.nextInt(0, 4)]
            }
        }
        println(str)
        return Pair(str, terminalsList)
    }


    private fun hasIncorrectTerminal(str : String, incorrectChar : Char) {
        val lexicalAnalyzer = createStringAnalyzer(str)
        try {
            while (lexicalAnalyzer.nextToken().terminal != Terminal.`$`) {
                //
            }
        } catch (e : ParseException) {
            println(e.message)
            assertTrue(e.message!!.contains(incorrectChar))
            return
        }
        fail("Mast have exception with incorrect char $incorrectChar!!!")
    }

    private fun correctTerminals(str: String, vararg terminals : Terminal) {
        val lex = createStringAnalyzer(str)
        terminals.forEach {
            assertEquals(it, lex.nextToken().terminal)
        }
    }

    private fun createStringAnalyzer(str : String) : Lexer {
        val lex = Lexer()
        lex.setInput(str.reader())
        return lex
    }
}