import java.util.*
import kotlin.collections.ArrayList
import kotlin.collections.HashSet
import kotlin.math.sqrt


fun main() {

    val n = readln().toInt()
    val graphV = Array(n) {
        Pair(0, 0)
    }
    for (i in 0 until  n) {
        val (x, y) = readln().split(" ").map { it.toInt() }
        graphV[i] = Pair(x, y)
    }
    val d = Array(n) {
        -10000000.0
    }
    val comporator = kotlin.Comparator<Int> { p1: Int, p2 : Int ->
        return@Comparator if (d[p1] != d[p2]) {
            d[p1].compareTo(d[p2])
        } else {
            p1.compareTo(p2)
        }
    }
    val queue = ArrayList<Int>()
    val flags = Array(n) {
        false
    }
    var weight = 0.0
    flags[0] = true
    //var min = T(0, 0, 10000000000.0)
    for (i in 1 until n) {
        val V = graphV[0]
        val I = graphV[i]
        val r = sqrt(((I.first - V.first) * (I.first - V.first) + (I.second - V.second) * (I.second - V.second)).toDouble())
        d[i] = r
        queue.add(i)
        //println(queue.size)
    }

    //На массиве, лучше на куче но...
   // queue.add(min)
    repeat(n - 1) {
        //println(queue)
        val v = queue.minWith(comporator)
        queue.remove(v)
        weight += d[v]
        val V = graphV[v]
        flags[v] = true
        for (i in 0 until n) {
            if (i == v) {
                continue
            }
            val I = graphV[i]
            val r1 = sqrt(((I.first - V.first) * (I.first - V.first) + (I.second - V.second) * (I.second - V.second)).toDouble())
            if (!flags[i]) {
                if (r1 < d[i]) {
                    d[i] = r1
                }
            }
        }
    }
    println(weight)
}