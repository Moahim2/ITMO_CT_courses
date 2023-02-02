



fun main() {
    val input1 = readln().split(" ")
    val m = input1[0].toInt()
    val n = input1[1].toInt()
    val graph = Array<Pair<Boolean, HashSet<Int>>>(m * n) {
        Pair(false, HashSet())
    }

    for (i in 0 until m) {
        val line = readln()
        for (j in 0 until n) {
            if (line[j] == 'O') {
                graph[i * n + j] = Pair(true, HashSet())
                if (i > 0) {
                    graph[(i - 1) * n + j].second.add(i * n + j)
                    graph[i * n + j].second.add((i - 1) * n + j)
                }
                if (i < m - 1) {
                    graph[(i + 1) * n + j].second.add(i * n + j)
                    graph[i * n + j].second.add((i + 1) * n + j)
                }
                if (j > 0) {
                    graph[i * n + j - 1].second.add(i * n + j)
                    graph[i * n + j].second.add(i * n + j - 1)
                }
                if (j < n - 1) {
                    graph[i * n + j + 1].second.add(i * n + j)
                    graph[i * n + j].second.add(i * n + j + 1)
                }
            }
        }
    }
    //println(graph[1].second)
    val vis = Array(m * n) {
        false
    }

    fun dfs(u : Int) {
        vis[u] = true
        for (v in graph[u].second) {
            if (graph[v].first) {
                if (!vis[v]) {
                    dfs(v)
                }
            }
        }
    }
    var count = 0
    for (i in 0 until n * m) {
        if (graph[i].first) {
            if (!vis[i]) {
              dfs(i)
              count++
            }
        }
    }
    println(count)
}
