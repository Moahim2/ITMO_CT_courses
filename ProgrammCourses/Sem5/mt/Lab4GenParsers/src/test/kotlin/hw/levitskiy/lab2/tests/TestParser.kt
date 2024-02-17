package hw.levitskiy.lab2.tests

import hw.levitskiy.`$generating$`.lab2.Parser
import hw.levitskiy.`$generating$`.lab2.Terminal
import hw.levitskiy.`$generating$`.lab2.Tree
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.fail
import java.text.ParseException

/**
 * Для проверки полноты всех правил будем идти от наименьшего набора правил к наибольшему добавляя поочередно.
 * Для проверки корректности получим всевозможные ошибки из парсера.
 */
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
            Terminal.VAR.name, "S'"
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
            Terminal.NAME.name, Terminal.COL.name,
            Terminal.ARR.name, Terminal.LBR.name, "E", Terminal.RBR.name,
            Terminal.SCOL.name
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
            Terminal.ARR.name, Terminal.COL.name,
            Terminal.ARR.name, Terminal.LBR.name, "E", Terminal.RBR.name,
            Terminal.SCOL.name
        )
    }




    /**
     * Проверка правила E -> nE'
     */
    @Test
    fun `test_E-nE'`() {
        val tree = parseExample("var x : Array<Int>;").children[1]
        correctSubtree(
            tree.children[4],
            "E",
            Terminal.NAME.name, "E'"
        )
    }

    /**
     * Проверка правила E' -> eps
     */
    @Test
    fun `test_E'-eps`() {
        val tree = parseExample("var x : Array<Int>;").children[1]
        correctSubtree(tree.children[4].children[1], "E'")
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
        val tree = parseExample("var x : Array<List<Int>>;").children[1]
        correctSubtree(
            tree.children[4].children[1],
            "E'",
            Terminal.LBR.name, "T", Terminal.RBR.name
        )
    }

    /**
     * Проверка правила T -> ET'
     */
    @Test
    fun `test_T-ET'`() {
        val tree = parseExample("var x : Array<List<Int>>;").children[1]
        correctSubtree(
            tree.children[4].children[1].children[1],
            "T",
            "E", "T'"
        )
    }


    /**
     * Проверка правила T' -> eps
     */
    @Test
    fun `test_T'-eps`() {
        val tree = parseExample("var x : Array<List<Int>>;").children[1]
        correctSubtree(
            tree.children[4].children[1].children[1].children[1],
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
        val tree = parseExample("var Array : Array<Array<Int>>;").children[1]
        correctSubtree(
            tree.children[4],
            "E",
            Terminal.ARR.name, Terminal.LBR.name, "E", Terminal.RBR.name
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
        val tree = parseExample("var maps : Array<Map<Int, Int>>;").children[1]
        correctSubtree(
            tree.children[4].children[1].children[1].children[1],
            "T'",
            Terminal.COM.name, "E", "T'"
        )
    }

    @Test
    fun testEasyDifferentCombinations() {
        parseExample("var x : Array<Ex<I, I, I>>;")
        parseExample("var x : Array<HardClass<A,B,C,D,E,F,G,H,J,K,L,M,N,O,P,Q,R, S, T, U,X,Y,Z>>;")
        parseExample("var x : Array<Array<Map<Int, Pair<String,String>>>>;")
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
        checkException("x: Array<Int>;", Terminal.NAME, Terminal.VAR)
    }

    @Test
    fun test_noNameOfArray() {
        checkException("var : Array<Int>;", Terminal.COL, Terminal.ARR, Terminal.NAME)
    }

    @Test
    fun test_noCol() {
        checkException("var x Array<Int>;", Terminal.ARR, Terminal.COL)
    }

    @Test
    fun test_noStartKeynameArray() {
        checkException("var x : <Int>;", Terminal.LBR, Terminal.ARR)
    }

    @Test
    fun test_noStartLBR() {
        checkException("var x : Array Int>;", Terminal.NAME, Terminal.LBR)
    }

    @Test
    fun test_noStartRBR() {
        checkException("var x : Array <Int;", Terminal.SCOL, Terminal.RBR)
        checkException("var x : Array <Map,> ;", Terminal.COM, Terminal.RBR)
        checkException("var x : Array<Int, Int> ;", Terminal.COM, Terminal.RBR)
    }

    @Test
    fun test_noSCOL() {
        checkException("var x : Array <Int>", Terminal.`$`, Terminal.SCOL)
    }

    @Test
    fun test_badETerminals() {
        checkException("var x : Array <>;", Terminal.RBR)
        checkException("var x : Array <,>;", Terminal.COM)
        checkException("var x : Array <<>;", Terminal.LBR, Terminal.ARR, Terminal.NAME)
        checkException("var x : Array<Array Int>;", Terminal.NAME, Terminal.LBR)
        checkException("var x : Array<Array <Int;", Terminal.SCOL, Terminal.RBR)
        ///!!! Важно!
        checkException("var x : Array<Array>;", Terminal.RBR, Terminal.LBR)
    }

    @Test
    fun `test_badE'Terminals`() {
        checkException("var x : Array<Map Int>;", Terminal.NAME, Terminal.LBR, Terminal.RBR, Terminal.COM)
        checkException("var x : Array<Map<Int>;", Terminal.SCOL, Terminal.RBR)
    }

    @Test
    fun test_badTTerminals() {
        checkException("var x : Array<Map<,>>;", Terminal.COM)
        checkException("var x : Array<Array<var>>;", Terminal.VAR, Terminal.ARR, Terminal.NAME)
    }

    @Test
    fun `test_badT'Terminals`() {
        checkException("var x : Array<Map<Int String>>;", Terminal.NAME)
        checkException("var x : Array<Map<Int, String;", Terminal.SCOL, Terminal.COM, Terminal.RBR)
        checkException("var x : Array<Map<Pair<Int, Int>>;", Terminal.SCOL, Terminal.RBR)
    }


    private fun checkException(example : String, errorTerminal : Terminal, vararg expectedTerminals: Terminal) {
        try {
            parseExample(example)
        } catch (e : ParseException) {
            assertTrue(e.message!!.contains(errorTerminal.toString()))
            expectedTerminals.forEach {
                assertTrue(e.message!!.contains(it.name))
            }
            return
        }
        fail("Must be exception, because incorrect input!!!")
    }


    private fun parseExample(str : String) : Tree {
        return parser.parse(str.reader()).second
    }


    private fun correctSubtree(tree: Tree, nameV : String, vararg children : String) {
        assertEquals(nameV, tree.node)
        assertArrayEquals(children, tree.children.stream().map(Tree::node).toArray())
    }

}