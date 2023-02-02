import java.util.*
import kotlin.collections.HashSet


fun main() {
    val input = readln().split(" ")
    val n = input[0].toInt()
    val m = input[1].toInt()
    val s = input[2].toInt() - 1
    val t = input[3].toInt() - 1

    val graph = Array<HashSet<Pair<Int, Int>>>(n) {
        HashSet()
    }

    for (i in 0 until m) {
        val line = readln().split(" ")
        val b = line[0].toInt() - 1
        val e = line[1].toInt() - 1
        val w = line[2].toInt()
        graph[b].add(Pair(e, w))
    }

    val d = Array(n) {
        2000 * n + 1
    }

    d[s] = 0
    for (i in 0 until n) {
        for (u in 0 until n) {
            if (d[u] == 2000 * n + 1) {
                continue
            }
            for (v in graph[u]) {
                if (d[v.first] > d[u] + v.second) {
                    d[v.first] = d[u] + v.second
                }
            }
        }
    }


    if (d[t] == 2000 * n + 1) {
        println("Unreachable")
    } else {
        println(d[t])
    }
}