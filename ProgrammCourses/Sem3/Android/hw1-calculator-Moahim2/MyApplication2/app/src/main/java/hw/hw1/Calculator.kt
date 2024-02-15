package hw.hw1

import java.math.BigDecimal
import java.math.RoundingMode
import java.text.NumberFormat
import java.util.*

class Calculator() {

    constructor(term : String, isEqv : Boolean) : this() {
        if (term != "") {
            val lineCommands = term.split(" ").joinToString("")
            for (i in 1..lineCommands.length) {
                parse(lineCommands[i - 1])
            }
            if (isEqv && calculatorOutput != "0") {
                typeParsing = Parsers.ParseEq
            }
        }
    }

    enum class Parsers {
        ParseConst, ParseBinary, ParseEq
    }
    private val redactFormat = NumberFormat.getInstance(Locale.ENGLISH)

    private val tableSignsOfOperation = arrayOf('+', '-', '*', '/')
    private var typeParsing = Parsers.ParseConst
    private var curVal = BigDecimal(0)
    private var rightOperand = BigDecimal(0)
    private var inputNewVal = ""
    private var typeArithmeticOperation : Char? = null
    private var lastSymbol : Char? = null
    private var correctlyOperation : Boolean = true
    private var exceptionMessage : String = ""

    var calculatorOutput : String = ""

    private fun parseConst(ch : Char) {
        when (ch) {
            in tableSignsOfOperation -> {
                if (lastSymbol == '.' || lastSymbol in tableSignsOfOperation) {
                    createExceptionMessage("Invalid character now")
                    return
                }

                if (inputNewVal == "") {
                    if (ch == '-') { //start minus
                        inputNewVal += ch
                    } else {
                        createExceptionMessage("Invalid character now")
                    }
                    return
                }
                typeParsing = Parsers.ParseBinary
                if (typeArithmeticOperation == null) {
                    curVal = inputNewVal.toBigDecimal()
                } else {
                    rightOperand = inputNewVal.toBigDecimal()
                }
                typeArithmeticOperation = ch
                inputNewVal = ""
            }
            '.' -> {
                if (inputNewVal.contains(".") || inputNewVal == "") {
                    createExceptionMessage("Invalid character now")
                    return
                }
                inputNewVal += ch
            }
            '=' -> {
                createExceptionMessage("Invalid character now")
                return
            }
            else -> {
                if (calculatorOutput == "0") {
                    inputNewVal += '.'
                }
                inputNewVal += ch //isDigit
            }
        }
    }


    private fun parseBinary(ch: Char) {
        when (ch) {
            in tableSignsOfOperation, '=' -> {
                if (lastSymbol in tableSignsOfOperation || lastSymbol == '.' || lastSymbol == '=') {
                    createExceptionMessage("Invalid character now")
                    return
                }

                rightOperand = inputNewVal.toBigDecimal()
                when (typeArithmeticOperation) {
                    '+' -> curVal = curVal.add(rightOperand)
                    '-' -> curVal = curVal.subtract(rightOperand)
                    '*' -> curVal = curVal.multiply(rightOperand)
                    '/' -> if (rightOperand == BigDecimal(0)) {
                        calculatorOutput = calculatorOutput.substring(
                            0, calculatorOutput.length - inputNewVal.length)
                        createExceptionMessage("Error, cannot be divided by 0")
                        inputNewVal = ""
                        lastSymbol = '/'
                        return
                    } else {
                        curVal = redactFormat.format(
                            curVal.divide(rightOperand, 15, RoundingMode.HALF_EVEN)).replace(
                            ",", "").toBigDecimal()
                    }
                }
                inputNewVal = ""
                typeArithmeticOperation = ch
                if (ch == '=') {
                    typeParsing = Parsers.ParseEq
                }
            }
            else -> { //isDigit
                parseConst(ch)
            }
        }
    }


    private fun parseEq(ch : Char) {
        when(ch) {
            in tableSignsOfOperation -> {
                inputNewVal = ""
                rightOperand = BigDecimal(0)
                typeArithmeticOperation = ch
                typeParsing = Parsers.ParseBinary
            }
            '=', '.' -> {
                createExceptionMessage("Invalid character now")
                return
            }
            else -> { //isDigit
                clearAll()
                parseConst(ch)
            }
        }
    }


    fun parse(ch: Char) : Boolean {
        when (ch) {
            'C' -> { clearAll(); return true }
            'c' -> { return true }
            else -> {
                when (typeParsing) {
                    Parsers.ParseConst ->parseConst(ch)
                    Parsers.ParseBinary -> parseBinary(ch)
                    Parsers.ParseEq -> parseEq(ch)
                }
            }
        }
        if (!correctlyOperation) {
            correctlyOperation = true
            return false
        }
        redactingOutput(ch)
        return true
    }

    private fun redactingOutput(ch : Char) {
        when(ch) {
            in tableSignsOfOperation -> calculatorOutput += " $ch "
            '=' -> calculatorOutput = curVal.toString()
            else -> calculatorOutput += ch
        }
        lastSymbol = ch
    }

    private fun clearAll() {
        typeParsing = Parsers.ParseConst
        curVal = BigDecimal(0)
        rightOperand = BigDecimal(0)
        inputNewVal = ""
        typeArithmeticOperation = null
        lastSymbol = null
        correctlyOperation = true
        calculatorOutput = ""
    }

    private fun createExceptionMessage(string : String) {
        exceptionMessage = string
        correctlyOperation = false
    }

    fun takeFlagEqv() : Boolean {
        return typeParsing == Parsers.ParseEq
    }

    fun takeExceptionMessage() : String {
        val tmp = exceptionMessage
        exceptionMessage = ""
        return tmp
    }

}