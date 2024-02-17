package test

import org.junit.Assert
import org.junit.FixMethodOrder
import org.junit.Test
import org.junit.runners.MethodSorters
import parser.Parser
import parser.Token
import parser.Tree
import java.text.ParseException

/**
 * Для проверки полноты всех правил будем идти от наименьшего набора правил к наибольшему добавляя поочередно.
 * Для проверки корректности получим всевозможные ошибки из парсера.
 * (все чутка закостылилось и уже неверно по некоторому модулю в плане комментариев после модификации)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class TestParser {
    private val parser = Parser()




    /**
     * S -> var S' -- есть всегда в любом примере, т.к. это единственное правило из стартового нетерминала
     *
     * S' -> n/Array : Array<E> ; $ -- тоже
     * =>
     * E -> nE' -- тоже должно быть, т.к. оно единственное из E
     * =>
     * E' -> eps -- также должно быть так как оно одно из двух и самое короткое.
     * =>
     * "var n: Array<n>" / "var Array: Array<n>" / - Минимальные примеры использования правила из S.
     */


    /**
     * Проверка правила S -> var S'
     */
    @Test
    fun test_S() {
        val tree = parseExample("var x : Array<Int>;")
        correctSubtree(
            tree,
            "S",
            Token.VAR.word, "S'"
        )
    }

    /**
     * Проверка правила S' -> n : Array<E> ;
     */
    @Test
    fun `test_S'1`() {
        val tree = parseExample("var x : Array<Int>;")
        correctSubtree(
            tree.children[1],
            "S'",
            Token.NAME.word, Token.COL.word , "S''"
        )
    }

    /**
     * Проверка правила S' -> Array : Array<E> ;
     */
    @Test
    fun `test_S'2`() {
        val tree = parseExample("var Array : Array<Int>;")
        correctSubtree(
            tree.children[1],
            "S'",
            Token.ARR.word, Token.COL.word, "S''"
        )
    }




    /**
     * Проверка правила E -> nE'
     */
    @Test
    fun `test_E-nE'`() {
        val tree = parseExample("var x : Array<Int>;").children[1].children[2]
        correctSubtree(
            tree.children[2],
            "E",
            Token.NAME.word, "E'"
        )
    }

    /**
     * Проверка правила E' -> eps
     */
    @Test
    fun `test_E'-eps`() {
        val tree = parseExample("var x : Array<Int>;").children[1].children[2]
        correctSubtree(tree.children[2].children[1], "E'")
    }


    /**
     * Далее добавление правила E' -> <T> влечет за собой раскрытие T
     * =>
     * T -> ET' также должно быть
     * =>
     * T' -> eps также обязано быть в следующем минимальном примере
     * =>
     * var n : Array<n<n>>;
     */


    /**
     * Проверка правила E' -> <T>
     */
    @Test
    fun `test_E'-lbr T rbr`() {
        val tree = parseExample("var x : Array<List<Int>>;").children[1].children[2]
        correctSubtree(
            tree.children[2].children[1],
            "E'",
            Token.LBR.word, "T", Token.RBR.word
        )
    }

    /**
     * Проверка правила T -> ET'
     */
    @Test
    fun `test_T-ET'`() {
        val tree = parseExample("var x : Array<List<Int>>;").children[1].children[2]
        correctSubtree(
            tree.children[2].children[1].children[1],
            "T",
            "E", "T'"
        )
    }


    /**
     * Проверка правила T' -> eps
     */
    @Test
    fun `test_T'-eps`() {
        val tree = parseExample("var x : Array<List<Int>>;").children[1].children[2]
        correctSubtree(
            tree.children[2].children[1].children[1].children[1],
            "T'",
        )
    }


    /**
     * Проверка правила E -> Array<E>
     * Для этого нам понадобится простейший пример :
     * var n/Array : Array<Array<Int>>;
     */
    @Test
    fun `test_E-Array lbr E rbr`() {
        val tree = parseExample("var Array : Array<Array<Int>>;").children[1].children[2]
        correctSubtree(
            tree.children[2],
            "E",
            Token.ARR.word, Token.LBR.word, "E", Token.RBR.word
        )
    }



    /**
     * К этому времени у нас осталось одно непроверенное правило -
     * T' -> ,ET'
     * Очевидно минимальным примером его использования будет:
     * var n : Array<Map<Int, Int>>
     */

    /**
     * Проверка правила T' -> ,ET'
     */
    @Test
    fun `test_T'-,ET'`() {
        val tree = parseExample("var maps : Array<CLASS<Int, Int>>;").children[1].children[2]
        correctSubtree(
            tree.children[2].children[1].children[1].children[1],
            "T'",
            Token.COM.word, "E", "T'"
        )
    }

    @Test
    fun testEasyDifferentCombinations() {
        parseExample("var x : Array<Ex<I, I, I>>;")
        parseExample("var x : Array<HardClass<A,B,C,D,E,F,G,H,J,K,L,M,N,O,P,Q,R, S, T, U,X,Y,Z>>;")
        parseExample("var x : Array<Array<Class<Int, Pair<String,String>>>>;")
        parseExample("var x : Array<Ex<I, Class<Class<I, I, I>,Class<I, I, I>,Class<I, I, I>>, I>>;")
    }

    @Test
    fun testHardDifferentCombinations() {
        parseExample("var Array : Array<Array<Array<Array<Array<Array<Class<A,A,A,A,A,A,A,A,A,A,A,A,A,A>>>>>>>;")
        parseExample("var x : Array<Ex<Array<List<Class<A,B,C,D,E<T,K>>>>, K, Array<Array<Pair<Pair<Int, Char>,Int>>>>>;")
        parseExample("var randomName: Array<List<Array<Array<List<List<Array<Array<Array<Array<Array<List<Pair<Array<Int>,Array<Int>>>>>>>>>>>>>>;")
    }




    @Test
    fun test_noVar() {
        checkException("x: Array<Int>;", Token.NAME, Token.VAR)
    }

    @Test
    fun test_noNameOfArray() {
        checkException("var : Array<Int>;", Token.COL, Token.ARR, Token.NAME, Token.MAP)
    }

    @Test
    fun test_noCol() {
        checkException("var x Array<Int>;", Token.ARR, Token.COL)
    }

    @Test
    fun test_noStartKeyWordArray() {
        checkException("var x : <Int>;", Token.LBR, Token.ARR)
    }

    @Test
    fun test_noStartLBR() {
        checkException("var x : Array Int>;", Token.NAME, Token.LBR)
    }

    @Test
    fun test_noStartRBR() {
        checkException("var x : Array <Int;", Token.SCOL, Token.RBR)
        checkException("var x : Array <MapT,> ;", Token.COM, Token.RBR)
        checkException("var x : Array<Int, Int> ;", Token.COM, Token.RBR)
    }

    @Test
    fun test_noSCOL() {
        checkException("var x : Array <Int>", Token.END, Token.SCOL)
    }

    @Test
    fun test_badETokens() {
        checkException("var x : Array <>;", Token.RBR)
        checkException("var x : Array <,>;", Token.COM)
        checkException("var x : Array <<>;", Token.LBR, Token.ARR, Token.NAME)
        checkException("var x : Array<Array Int>;", Token.NAME, Token.LBR)
        checkException("var x : Array<Array <Int;", Token.SCOL, Token.RBR)
        ///!!! Важно!
        checkException("var x : Array<Array>;", Token.RBR, Token.LBR)
    }

    @Test
    fun `test_badE'Tokens`() {
        checkException("var x : Array<MapT Int>;", Token.NAME, Token.LBR, Token.RBR, Token.COM)
        checkException("var x : Array<MapT<Int>;", Token.SCOL, Token.RBR)
    }

    @Test
    fun test_badTTokens() {
        checkException("var x : Array<MapT<,>>;", Token.COM)
        checkException("var x : Array<Array<var>>;", Token.VAR, Token.ARR, Token.NAME)
    }

    @Test
    fun `test_badT'Tokens`() {
        checkException("var x : Array<MapT<Int String>>;", Token.NAME)
        checkException("var x : Array<MapT<Int, String;", Token.SCOL, Token.COM, Token.RBR)
        checkException("var x : Array<MapT<Pair<Int, Int>>;", Token.SCOL, Token.COM, Token.RBR)
    }


    private fun checkException(example : String, errorToken : Token, vararg expectedTokens: Token) {
        try {
            parseExample(example)
        } catch (e : ParseException) {
            Assert.assertTrue(e.message!!.contains(errorToken.toString()))
            expectedTokens.forEach {
                Assert.assertTrue(e.message!!.contains(it.word))
            }
            return
        }
        Assert.fail("Must be exception, because incorrect input!!!")
    }


    private fun parseExample(str : String) : Tree {
        return parser.parse(str.reader())
    }


    private fun correctSubtree(tree: Tree, nameV : String, vararg children : String) {
        Assert.assertEquals(nameV, tree.node)
        Assert.assertArrayEquals(children, tree.children.stream().map(Tree::node).toArray())
    }

}
