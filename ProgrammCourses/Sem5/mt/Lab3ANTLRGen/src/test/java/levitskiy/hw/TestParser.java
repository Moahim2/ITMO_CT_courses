package levitskiy.hw;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static levitskiy.hw.FileTranslator.translateAndSaveToFile;

@TestMethodOrder(MethodOrderer.DisplayName.class)
public class TestParser {

    private static final String OUT_PREFIX = "src/test/java/levitskiy/hw/out/";

    private static final String DEFAULT_NAME = OUT_PREFIX + "test.kt";

    /**
     * Переменная понимает свой тип если ей что-нибудь где-нибудь присвоить
     */


    @Test
    public void exampleFiles() {
        compileTest(
                OUT_PREFIX + "hello1.kt",
                "if == 5 5 if == 5 * + 1 2 3 print 4 print < 2 3"
        );

        //while == 5 5 a1 a2 a3 a4 a5 a6
        compileTest(
                OUT_PREFIX + "hello2.kt",
                "= x 5 if > x 3 = x / 7 10 print x if < x 5 print FALSE if == x 5 = y 10 print y"
        );
    }

    @Test
    public void exampleWhiles() {
        compileTest(
                OUT_PREFIX + "helloWhile1.kt",
                "while TRUE (do + 5 5)"
        );

        compileTest(
                OUT_PREFIX + "helloWhile2.kt",
                "= x 10 while < 10 x (do = x + x 1)"
        );

        compileTest(
                OUT_PREFIX + "helloWhile3.kt",
                "= x 0 while < 10 x (do = x + x 1 print x)"
        );

        compileTest(
                OUT_PREFIX + "helloWhile4.kt",
                "= x 1 while < 10 x (do = y 5 while > y 0 (do = y - y 1 = x + x 1) = x + x 1 print x)"
        );
    }


    @Test
    public void emptyProgram() {
        compileTestWithDefaultName("");
    }

    @Test
    public void simpleTests() {
        compileTestWithDefaultName("+ 1 2");
        compileTestWithDefaultName("* 2 3");
        compileTestWithDefaultName("/ 10 1000");
        compileTestWithDefaultName("- 100000000 100000000000");
        compileTestWithDefaultName("print 1");
        compileTestWithDefaultName("= x 5");
        compileTestWithDefaultName("= x TRUE");
        compileTestWithDefaultName("= x < 5 3");
        compileTestWithDefaultName("if < 5 3 print 10");
        compileTestWithDefaultName("if < 5 3 if == 2 2 = y - 10 2 print y");

        compileTestWithDefaultName("= x 3 + x 3");
    }

    @Test
    public void manyOperations() {
        compileTestWithDefaultName("+ - 1 2 3");
        compileTestWithDefaultName("+ -1 2 + 1 2 + 1 -2");
    }

    @Test
    public void variableWithoutType() {
        testTranslationException("print x");
    }

    @Test
    public void variableIncompatibleTypes() {
        testTranslationException("= x TRUE if x = x 3");
    }


    @Test
    public void assignment() {
        compileTestWithDefaultName("= x 3 = z TRUE = y x = w z");
    }


    @Test
    public void whitespaces() {
        compileTestWithDefaultName("if < 5 3 if ! < 5 3 = x + 5 4 \n = \n \n \t\t xxx 4 \nprint x");
    }

    @Test
    public void hard() {
        compileTestWithDefaultName("= x 3 if < x 3 if TRUE if FALSE if > 5 x = y < / 1 2 7 print x print y print TRUE");
        compileTestWithDefaultName("= name1 TRUE if == 5 5 if name1 print ! name1");
    }



    private void compileTestWithDefaultName(String program) {
        compileTest(DEFAULT_NAME, program);
    }

    private void compileTest(String outFileName, String program) {
        translateAndSaveToFile(outFileName, program, true);
        Assertions.assertTrue(KotlinCompiler.compile(outFileName));
    }

    private void testTranslationException(String program) {
        Assertions.assertThrows(
                TranslationException.class,
                () -> translateAndSaveToFile(DEFAULT_NAME, program, true)
        );
    }


}
