
import java.util.*


fun main() {
    val n = readln().toInt()
    val codes = ArrayList<Int>()

    val input = readln().split(" ")
    val types = HashSet<Int>()
    val count = HashMap<Int, Int>()

    for (i in 1..n) {
        count[i] = 0
    }

    for (i in 1..n-2) {
        val x = input[i - 1].toInt()
        types.add(x)
        codes.add(x)
        count[x] = count[x]!! + 1
    }

    val lists = TreeSet<Int>()
    for (i in 1..n) {
        if (!types.contains(i)) {
            lists.add(i)
        }
    }

    for (i in 0..n-3) {
        val l = lists.first()
        val v = codes[i]
        count[v] = count[v]!! - 1
        println("$l $v")
        lists.remove(l)

        if (count[v] == 0) {
            lists.add(v)
        }
    }

    println("${lists.first()} ${lists.last()}")
}