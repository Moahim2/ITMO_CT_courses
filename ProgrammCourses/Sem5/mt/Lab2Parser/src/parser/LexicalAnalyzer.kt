package parser

import java.io.IOException
import java.io.Reader
import java.text.ParseException

class LexicalAnalyzer(private var input: Reader) {
    private var curChar : Char = ' '
    var curPos = 0

    private var curToken: Token? = null

    init {
        nextChar()
    }

    @Throws(ParseException::class)
    private fun nextChar() {
        curPos++
        curChar = try {
            input.read().toChar()
        } catch (e: IOException) {
            throw ParseException(e.message, curPos)
        }
    }

    private fun isBlank(c: Char): Boolean {
        return c == ' ' || c == '\r' || c == '\n' || c == '\t'
    }

    @Throws(ParseException::class)
    fun nextToken() : Token {
        while (isBlank(curChar)) {
            nextChar()
        }

        curToken = when {
            curChar.isLetter() -> {
                val sb = StringBuilder()
                do {
                    sb.append(curChar)
                    nextChar()
                } while (curChar.isLetter())

                when (sb.toString()) {
                    "var" -> Token.VAR
                    "Array" -> Token.ARR
                    "Map" -> Token.MAP
                    else -> Token.NAME
                }
            }
            else -> {
                val token = when (curChar) {
                    ':' -> Token.COL
                    ',' -> Token.COM
                    ';' -> Token.SCOL
                    '<' -> Token.LBR
                    '>' -> Token.RBR
                    (-1).toChar() -> Token.END
                    else -> throw ParseException("Illegal character $curChar", curPos)
                }
                nextChar()
                token
            }
        }
        return curToken!!
    }

    fun curToken() : Token = curToken!!
}
