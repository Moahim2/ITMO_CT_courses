package test

import org.junit.Assert
import org.junit.Test
import parser.LexicalAnalyzer
import parser.Token
import java.text.ParseException
import java.util.*
import kotlin.collections.ArrayList

class TestLexicalAnalyzer {
    private val random = Random(4875043285743285204L)
    private val correctWhitespaces = listOf("\t", "\r", "\n", " ")
    private val correctTokens = Token.values()


    /**
     * Проверка всех токенов по отдельности.
     */
    @Test
    fun test1CorrectFullTokens() {
        correctTokens.forEach {
            val str = if (it != Token.END) {
                it.word
            } else {
                (-1).toChar().toString()
            }
            correctTokens(str, it)
        }
    }

    /**
     * Проверка некоторых токенов вместе с корректными пробельными символами.
     */
    @Test
    fun test2CorrectWhitespaces() {
        correctTokens(" ")
        correctTokens("      var     ", Token.VAR)
        correctTokens("\tarray         \r\n", Token.NAME)
        correctTokens("\n \r \t \n \r         :         \t \n \r", Token.COL)
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
                    str += Token.NAME.word
                }
            }
            correctTokens(str, Token.NAME)
        }
    }

    /**
     * Проверка совокупности корректных токенов и пробельных символов
     */
    @Test
    fun test4CorrectTokenList() {
        correctTokens(
            "var x : Array<Int>;", Token.VAR, Token.NAME, Token.COL, Token.ARR,
            Token.LBR, Token.NAME, Token.RBR, Token.SCOL, Token.END
        )
        correctTokens(
            "var var var Array var :::,,,,             Int,          \tName,;;;;<<<           >>>",
            Token.VAR, Token.VAR, Token.VAR, Token.ARR, Token.VAR, Token.COL, Token.COL, Token.COL,
            Token.COM, Token.COM, Token.COM, Token.COM, Token.NAME, Token.COM, Token.NAME, Token.COM,
            Token.SCOL, Token.SCOL, Token.SCOL, Token.SCOL, Token.LBR, Token.LBR, Token.LBR, Token.RBR,
            Token.RBR, Token.RBR, Token.END
        )
    }

    /**
     * Проверка рандомных последовательностей из корректных токенов.
     */
    @Test
    fun test5RandomCorrectTokenList() {
        repeat(10) {
            val (str, tokensList) = generateRandomCorrectTask()
            correctTokens(str, *tokensList.toTypedArray())
        }
    }

    /**
     * Проверка на непринятие некорректных токенов
     */
    @Test
    fun test6IncorrectTokens() {
        hasIncorrectToken("\'", '\'')
        hasIncorrectToken("var x : Array<11231231231231Int>;", '1')
        hasIncorrectToken("var ---------- x : Array<Int>;", '-')
    }


    /**
     * Рандомная проверка на непринятие некорректных токенов
     */
    @Test
    fun test7RandomIncorrectTokens() {
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
            hasIncorrectToken(str1 + ch + str2, ch)
        }
    }





    private fun generateRandomCorrectTask() : Pair<String, List<Token>> {
        val tokensList = ArrayList<Token>()
        var str = ""
        repeat(random.nextInt(1, 1000)) {
            val curToken = correctTokens[random.nextInt(0, 8)]
            tokensList.add(curToken)
            str += curToken.word
            if (curToken == Token.VAR || curToken == Token.NAME || curToken == Token.ARR || curToken == Token.MAP) {
                str += correctWhitespaces[random.nextInt(0, 4)]
            }
        }
        return Pair(str, tokensList)
    }


    private fun hasIncorrectToken(str : String, incorrectChar : Char) {
        val lexicalAnalyzer = createStringAnalyzer(str)
        try {
            while (lexicalAnalyzer.nextToken() != Token.END) {
                //
            }
        } catch (e : ParseException) {
            Assert.assertTrue(e.message!!.contains(incorrectChar))
            return
        }
        Assert.fail("Mast have exception with incorrect char $incorrectChar!!!")
    }

    private fun correctTokens(str: String, vararg tokens : Token) {
        val lexicalAnalyzer = createStringAnalyzer(str)
        tokens.forEach {
            Assert.assertEquals(it, lexicalAnalyzer.nextToken())
        }
    }

    private fun createStringAnalyzer(str : String) : LexicalAnalyzer {
        return LexicalAnalyzer(str.reader())
    }
}
