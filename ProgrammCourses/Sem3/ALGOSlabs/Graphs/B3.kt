import kotlin.math.min

fun main() {
    val (n, m) = readln().split(" ").map { it.toInt() }

    val graph = Array<ArrayList<Int>>(n + m) {
        ArrayList()
    }

    val vis = Array(n + m) {
        false
    }

    val tin = Array(n + m) {
        -1
    }

    val up = Array(n + m) {
        -1
    }

    for (ind in 0 until m) {
        val (i, j, k) = readln().split(" ").map { it.toInt() - 1 }
        val v = n + ind
        graph[v].add(i)
        graph[v].add(j)
        graph[v].add(k)
        graph[k].add(v)
        graph[j].add(v)
        graph[i].add(v)
    }
    val ans = Array(n + m) {
        false
    }



    var ansCount = 0
    var T = 0
    fun dfs(v : Int, p : Int) {
        vis[v] = true
        tin[v] = T++
        up[v] = tin[v]
        var c = 0
        for (u in graph[v]) {
            if (u == p) {
                continue
            }
            if (!vis[u]) {
                dfs(u, v)
                c++
                up[v] = min(up[v], up[u])
                if (up[u] >= tin[v] && p != -1) {
                    ans[v] = true
                    ansCount++
                }
            } else {
                up[v] = min(up[v], tin[u])
            }
        }
        if (p == -1 && c > 1) {
            ans[v] = true
            ansCount++
        }
    }


    for (i in n until n + m) {
        if (!vis[i]) {
            dfs(i, -1)
        }
    }

    val ans2 = ArrayList<Int>()
    for (i in n until n + m) {
        if (ans[i]) {
            ans2.add(i - n + 1)
        }
    }
    /////
    println(ans2.size)
    println(ans2.joinToString("\n"))
}
