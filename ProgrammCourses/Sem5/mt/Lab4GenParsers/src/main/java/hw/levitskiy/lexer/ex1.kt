package hw.levitskiy.lexer

fun f(int: Int) {

}

fun main() {
    f({ var result = 1; for (i in 2..10) result *= i; result}.invoke())
}