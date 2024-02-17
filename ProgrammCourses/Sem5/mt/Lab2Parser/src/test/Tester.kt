package test

import org.junit.runner.JUnitCore
import kotlin.reflect.KClass

fun runTest(clazz: KClass<out Any>) : Long {
    val start = System.currentTimeMillis()
    JUnitCore().run(clazz.java)

    println("============================")
    val time = System.currentTimeMillis() - start

    System.out.printf("OK %s in %dms %n", clazz.java.name, time)
    return time
}

fun main() {
    val time1 = runTest(TestLexicalAnalyzer::class)
    val time2 = runTest(TestParser::class)

    println("====================================================================================")
    System.out.printf("OK %s in %dms %n", "Full Tests", time1 + time2)
}
