package hw.levitskiy.`$generating$`.lab2

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
    fun parse(input : Reader) : Pair<Nothing?, Tree> {
        lex.setInput(input)
        lex.nextToken()
        var result = `S`()
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


    private fun `E'`(): Pair<Nothing?, Tree> {
        var r = Tree("E'")
        val result = when(token().terminal) {
            Terminal.`COM`, Terminal.`RBR` -> {
                null
            }
            Terminal.`LBR` -> {
                val `LBR` = ensure(Terminal.LBR)
                r.addChild(Tree(Terminal.LBR))
                val `T` = r.addChild(`T`())
                val `RBR` = ensure(Terminal.RBR)
                r.addChild(Tree(Terminal.RBR))
                null
            }

            else -> throw createParseException(Terminal.`COM`, Terminal.`RBR`, Terminal.`LBR`)
        }
        return Pair(result, r)
    }


    private fun `T'`(): Pair<Nothing?, Tree> {
        var r = Tree("T'")
        val result = when(token().terminal) {
            Terminal.`RBR` -> {
                null
            }
            Terminal.`COM` -> {
                val `COM` = ensure(Terminal.COM)
                r.addChild(Tree(Terminal.COM))
                val `E` = r.addChild(`E`())
                val `T'` = r.addChild(`T'`())
                null
            }

            else -> throw createParseException(Terminal.`COM`, Terminal.`RBR`)
        }
        return Pair(result, r)
    }


    private fun `S`(): Pair<Nothing?, Tree> {
        var r = Tree("S")
        val result = when(token().terminal) {
            Terminal.`VAR` -> {
                val `VAR` = ensure(Terminal.VAR)
                r.addChild(Tree(Terminal.VAR))
                val `S'` = r.addChild(`S'`())
                null
            }

            else -> throw createParseException(Terminal.`VAR`)
        }
        return Pair(result, r)
    }


    private fun `T`(): Pair<Nothing?, Tree> {
        var r = Tree("T")
        val result = when(token().terminal) {
            Terminal.`ARR`, Terminal.`NAME` -> {
                val `E` = r.addChild(`E`())
                val `T'` = r.addChild(`T'`())
                null
            }

            else -> throw createParseException(Terminal.`ARR`, Terminal.`NAME`)
        }
        return Pair(result, r)
    }


    private fun `S'`(): Pair<Nothing?, Tree> {
        var r = Tree("S'")
        val result = when(token().terminal) {
            Terminal.`NAME` -> {
                val `NAME` = ensure(Terminal.NAME)
                r.addChild(Tree(Terminal.NAME))
                val `COL` = ensure(Terminal.COL)
                r.addChild(Tree(Terminal.COL))
                val `ARR` = ensure(Terminal.ARR)
                r.addChild(Tree(Terminal.ARR))
                val `LBR` = ensure(Terminal.LBR)
                r.addChild(Tree(Terminal.LBR))
                val `E` = r.addChild(`E`())
                val `RBR` = ensure(Terminal.RBR)
                r.addChild(Tree(Terminal.RBR))
                val `SCOL` = ensure(Terminal.SCOL)
                r.addChild(Tree(Terminal.SCOL))
                null
            }
            Terminal.`ARR` -> {
                val `ARR` = ensure(Terminal.ARR)
                r.addChild(Tree(Terminal.ARR))
                val `COL` = ensure(Terminal.COL)
                r.addChild(Tree(Terminal.COL))
                val `ARR1` = ensure(Terminal.ARR)
                r.addChild(Tree(Terminal.ARR))
                val `LBR` = ensure(Terminal.LBR)
                r.addChild(Tree(Terminal.LBR))
                val `E` = r.addChild(`E`())
                val `RBR` = ensure(Terminal.RBR)
                r.addChild(Tree(Terminal.RBR))
                val `SCOL` = ensure(Terminal.SCOL)
                r.addChild(Tree(Terminal.SCOL))
                null
            }

            else -> throw createParseException(Terminal.`ARR`, Terminal.`NAME`)
        }
        return Pair(result, r)
    }


    private fun `E`(): Pair<Nothing?, Tree> {
        var r = Tree("E")
        val result = when(token().terminal) {
            Terminal.`NAME` -> {
                val `NAME` = ensure(Terminal.NAME)
                r.addChild(Tree(Terminal.NAME))
                val `E'` = r.addChild(`E'`())
                null
            }
            Terminal.`ARR` -> {
                val `ARR` = ensure(Terminal.ARR)
                r.addChild(Tree(Terminal.ARR))
                val `LBR` = ensure(Terminal.LBR)
                r.addChild(Tree(Terminal.LBR))
                val `E` = r.addChild(`E`())
                val `RBR` = ensure(Terminal.RBR)
                r.addChild(Tree(Terminal.RBR))
                null
            }

            else -> throw createParseException(Terminal.`ARR`, Terminal.`NAME`)
        }
        return Pair(result, r)
    }

}
