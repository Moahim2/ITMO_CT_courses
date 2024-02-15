import java.util.*
import kotlin.collections.ArrayList
import kotlin.collections.HashSet
import kotlin.math.min


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
    val vis = Array(n) {
        false
    }
    val topSorted = ArrayList<Int>()
    fun dfs(u : Int) {
        vis[u] = true
        for (v in graph[u]) {
            if (!vis[v.first]) {
                dfs(v.first)
            }
        }
        topSorted.add(u)
    }
    for (i in 0 until n) {
        if (!vis[i]) {
            dfs(i)
        }
    }
    topSorted.reverse()
    //println(topSorted)
    for (i in 0 until n) {
        val u = topSorted[i]
        if (d[u] == 2000 * n + 1) {
            continue
        }
        for (vP in graph[u]) {
            val v = vP.first
            val w = vP.second
            d[v] = min(d[v], d[u] + w)
        }
    }



    if (d[t] == 2000 * n + 1) {
        println("Unreachable")
    } else {
        println(d[t])
    }
}