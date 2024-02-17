package parser

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

    constructor(nodeType : Token) : this(nodeType.word)

    fun addChild(child : Tree) {
        children.add(child)
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
    private lateinit var lex : LexicalAnalyzer


    private fun S() : Tree {
        return when(lex.curToken()) {
            Token.VAR -> {
                lex.nextToken()
                Tree("S", Tree(Token.VAR), `S'`())
            }
            else -> throw createParseException(Token.VAR)
        }
    }


    //S' -> n : S''
    //S'' -> Array<E>
    //S'' -> Map<E, E>
    //S' -> n : Map<E, E>
    //S' -> n : Array<E>

    private fun `S'`() : Tree {
        return when(lex.curToken()) {
            Token.NAME -> {
                if (lex.nextToken() != Token.COL) {
                    throw createParseException(Token.COL)
                }
                lex.nextToken()
                Tree("S'", Tree(Token.NAME), Tree(Token.COL), `S''`())
            }
            Token.ARR -> {
                if (lex.nextToken() != Token.COL) {
                    throw createParseException(Token.COL)
                }
                lex.nextToken()
                Tree("S'", Tree(Token.ARR), Tree(Token.COL), `S''`())
            }
            Token.MAP -> {
                if (lex.nextToken() != Token.COL) {
                    throw createParseException(Token.COL)
                }
                lex.nextToken()
                Tree("S'", Tree(Token.MAP), Tree(Token.COL), `S''`())
            }
            else -> throw createParseException(Token.NAME, Token.ARR, Token.MAP)
        }
    }

    private fun parseToken(token : Token, r : Tree, next: Boolean = true) {
        val curToken = if (next) {
            lex.nextToken()
        } else {
            lex.curToken()
        }

        if (curToken != token) {
            throw createParseException(token)
        }
        r.addChild(Tree(token))
    }


    private fun `S''`(): Tree {
        val r = Tree("S''")
        when (lex.curToken()) {
            Token.ARR -> {
                r.addChild(Tree(Token.ARR))

                parseToken(Token.LBR, r,)

                lex.nextToken()
                r.addChild(E())
            }
            Token.MAP -> {
                r.addChild(Tree(Token.MAP))

                parseToken(Token.LBR, r)
                lex.nextToken()
                r.addChild(E())

                parseToken(Token.COM, r, false)

                lex.nextToken()
                r.addChild(E())
            }
            else -> throw createParseException(Token.ARR, Token.MAP)
        }

        parseToken(Token.RBR, r, false)

        parseToken(Token.SCOL, r)

        parseToken(Token.END, r)
        lex.nextToken()

        return r
    }


    private fun E(): Tree {
        val typeToken = lex.curToken()
        return when (lex.curToken()) {
            Token.NAME -> {
                lex.nextToken()
                Tree("E", Tree(typeToken), `E'`())
            }
            Token.ARR -> {
                if (lex.nextToken() == Token.LBR) {
                    lex.nextToken()
                    val treeE = E()
                    if (lex.curToken() == Token.RBR) {
                        lex.nextToken()
                        Tree("E", Tree(Token.ARR), Tree(Token.LBR), treeE, Tree(Token.RBR))
                    } else {
                        throw createParseException(Token.RBR)
                    }
                } else {
                    throw createParseException(Token.LBR)
                }
            }
            else -> throw createParseException(Token.NAME, Token.ARR)
        }

    }

    private fun `E'`(): Tree {
        val r = Tree("E'")
        when (lex.curToken()) {
            Token.LBR -> {
                r.addChild(Tree(Token.LBR))
                lex.nextToken()
                r.addChild(T())
                if (lex.curToken() == Token.RBR) {
                    r.addChild(Tree(Token.RBR))
                } else {
                    throw createParseException(Token.RBR)
                }
                lex.nextToken()
            }
            Token.RBR, Token.COM -> {}
            else -> throw createParseException(Token.LBR, Token.RBR, Token.COM)
        }
        return r
    }

    private fun T(): Tree {
        return when (lex.curToken()) {
            Token.NAME, Token.ARR -> {
                Tree("T", E(), `T'`())
            }
            else -> throw createParseException(Token.NAME, Token.ARR)
        }
    }

    private fun `T'`(): Tree {
        val r = Tree("T'")
        when (lex.curToken()) {
            Token.COM -> {
                r.addChild(Tree(Token.COM))
                lex.nextToken()
                r.addChild(E())
                r.addChild(`T'`())
            }
            Token.RBR -> {}
            else -> throw createParseException(Token.COM, Token.RBR)
        }
        return r
    }


    private fun createParseException(vararg tokens : Token) : ParseException {
        var enumerationStr = ""
        for (i in tokens.indices) {
            enumerationStr += tokens[i].word
            if (i < tokens.size - 1) {
                enumerationStr += " or "
            }
        }
        return ParseException(
            "$enumerationStr expected, but was ${lex.curToken()} at position ",
            lex.curPos
        )
    }

    @Throws(ParseException::class)
    fun parse(input : Reader) : Tree {
        lex = LexicalAnalyzer(input)
        lex.nextToken()
        return S()
    }
}

