package hw.levitskiy.`$generating$`.calculator

import java.io.BufferedWriter
import java.io.Reader
import java.text.ParseException
import kotlin.jvm.Throws

class Tree (val node : String, vararg children : Tree) {

    companion object {
        var globalNumber = 0
    }

    var children : ArrayList<Tree>
    private val number = globalNumber++

    init {
        this.children = ArrayList(children.toList())
    }

    constructor(node: String) : this(node, *emptyArray())

    constructor(nodeType : Terminal) : this(nodeType.name)

    fun addChild(child : Tree) {
        children.add(child)
    }

    fun <T> addChild(pChild : Pair<T, Tree>) : T {
        children.add(pChild.second)
        return pChild.first
    }

    fun toDot(out : BufferedWriter) {
        if (node == "S") {
            childrenToDot(out)
        } else {
            childrenToDot(out)
        }
    }

    private fun childrenToDot(out : BufferedWriter) {
        out.write("\"${node + number}\"[label=\"${node}\"]")
        out.newLine()
        for (ch in children) {
            out.write("\"${node + number}\" -> \"${ch.node + ch.number}\";")
            out.newLine()
        }
        for (ch in children) {
            ch.toDot(out)
        }
    }
}




class Parser {
    private val lex = Lexer()

    private fun token() : Token {
        return lex.curToken()
    }

    @Throws(ParseException::class)
    fun parse(input : Reader) : Pair<Int, Tree> {
        lex.setInput(input)
        lex.nextToken()
        var result = `E`()
        if (token().terminal != Terminal.`$`) {
            throw createParseException(Terminal.`$`)
        }
        return result
    }

    private fun createParseException(vararg tokens : Terminal) : ParseException {
        var enumerationStr = ""
        for (i in tokens.indices) {
            enumerationStr += tokens[i]
            if (i < tokens.size - 1) {
                enumerationStr += " or "
            }
        }
        return ParseException(
            "$enumerationStr expected, but was ${lex.curToken().terminal} as text: ${lex.curToken().textVal} at position ",
            lex.curPos()
        )
    }

    private fun ensure(x : Terminal) : String {
        if (token().terminal == x) {
            val t = token().textVal
            lex.nextToken()
            return t
        } else {
            throw createParseException(x)
        }
    }


    private fun `F'`(acc : Int): Pair<Int, Tree> {
        var r = Tree("F'")
        val result = when(token().terminal) {
            Terminal.`FTR` -> {
                val `FTR` = ensure(Terminal.FTR)
                r.addChild(Tree(Terminal.FTR))
                val `F'` = r.addChild(`F'`({ var result = 1; for (i in 2..acc) result *= i; result}.invoke()))
                `F'`
            }
            Terminal.`DIV`, Terminal.`RB`, Terminal.`ADD`, Terminal.`SUB`, Terminal.`$`, Terminal.`MUL` -> {
                acc
            }

            else -> throw createParseException(Terminal.`DIV`, Terminal.`RB`, Terminal.`ADD`, Terminal.`SUB`, Terminal.`$`, Terminal.`FTR`, Terminal.`MUL`)
        }
        return Pair(result, r)
    }


    private fun `E'`(acc : Int): Pair<Int, Tree> {
        var r = Tree("E'")
        val result = when(token().terminal) {
            Terminal.`ADD` -> {
                val `ADD` = ensure(Terminal.ADD)
                r.addChild(Tree(Terminal.ADD))
                val `T` = r.addChild(`T`())
                val `E'` = r.addChild(`E'`(acc + `T`))
                `E'`
            }
            Terminal.`RB`, Terminal.`$` -> {
                acc
            }
            Terminal.`SUB` -> {
                val `SUB` = ensure(Terminal.SUB)
                r.addChild(Tree(Terminal.SUB))
                val `T` = r.addChild(`T`())
                val `E'` = r.addChild(`E'`(acc - `T`))
                `E'`
            }

            else -> throw createParseException(Terminal.`RB`, Terminal.`ADD`, Terminal.`SUB`, Terminal.`$`)
        }
        return Pair(result, r)
    }


    private fun `T'`(acc : Int): Pair<Int, Tree> {
        var r = Tree("T'")
        val result = when(token().terminal) {
            Terminal.`DIV` -> {
                val `DIV` = ensure(Terminal.DIV)
                r.addChild(Tree(Terminal.DIV))
                val `F` = r.addChild(`F`())
                val `T'` = r.addChild(`T'`(acc / `F`))
                `T'`
            }
            Terminal.`MUL` -> {
                val `MUL` = ensure(Terminal.MUL)
                r.addChild(Tree(Terminal.MUL))
                val `F` = r.addChild(`F`())
                val `T'` = r.addChild(`T'`(acc * `F`))
                `T'`
            }
            Terminal.`RB`, Terminal.`ADD`, Terminal.`SUB`, Terminal.`$` -> {
                acc
            }

            else -> throw createParseException(Terminal.`DIV`, Terminal.`RB`, Terminal.`ADD`, Terminal.`SUB`, Terminal.`$`, Terminal.`MUL`)
        }
        return Pair(result, r)
    }


    private fun `T`(): Pair<Int, Tree> {
        var r = Tree("T")
        val result = when(token().terminal) {
            Terminal.`LB`, Terminal.`NUM` -> {
                val `F` = r.addChild(`F`())
                val `T'` = r.addChild(`T'`(`F`))
                `T'`
            }

            else -> throw createParseException(Terminal.`LB`, Terminal.`NUM`)
        }
        return Pair(result, r)
    }


    private fun `E`(): Pair<Int, Tree> {
        var r = Tree("E")
        val result = when(token().terminal) {
            Terminal.`LB`, Terminal.`NUM` -> {
                val `T` = r.addChild(`T`())
                val `E'` = r.addChild(`E'`(`T`))
                `E'`
            }

            else -> throw createParseException(Terminal.`LB`, Terminal.`NUM`)
        }
        return Pair(result, r)
    }


    private fun `F`(): Pair<Int, Tree> {
        var r = Tree("F")
        val result = when(token().terminal) {
            Terminal.`LB` -> {
                val `LB` = ensure(Terminal.LB)
                r.addChild(Tree(Terminal.LB))
                val `E` = r.addChild(`E`())
                val `RB` = ensure(Terminal.RB)
                r.addChild(Tree(Terminal.RB))
                val `F'` = r.addChild(`F'`(`E`))
                `F'`
            }
            Terminal.`NUM` -> {
                val `NUM` = ensure(Terminal.NUM)
                r.addChild(Tree(Terminal.NUM))
                val `F'` = r.addChild(`F'`(`NUM`.toInt()))
                `F'`
            }

            else -> throw createParseException(Terminal.`LB`, Terminal.`NUM`)
        }
        return Pair(result, r)
    }

}
