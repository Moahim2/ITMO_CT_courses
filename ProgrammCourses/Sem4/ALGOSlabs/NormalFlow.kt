import kotlin.math.min

private class EdmondsKarp(private val g: ArrayList<HashSet<Int>>, private val c : Array<Array<ArrayList<Long>>>) {
    val f = Array(c.size) {
        Array(c.size) {
            ArrayList<Long>()
        }
    }


    init {
        for (i in c.indices) {
            for (j in c[i].indices) {
                repeat(c[i][j].size) {
                    f[i][j].add(0L)
                }
            }
        }
    }


    private fun bfs(start: Int, end : Int, paths : Array<Pair<Int, Int>>) : Long {
        for (i in paths.indices) {
            paths[i] = Pair(-1, -100)
        }
        paths[start] = Pair(-2, -100)
        val queue = ArrayDeque<Pair<Int, Long>>()
        queue.addLast(Pair(start, Long.MAX_VALUE))

        while (queue.isNotEmpty()) {
            val (u, flow) = queue.first()
            queue.removeFirst()
            for (v in g[u]) {
                if (paths[v].first == -1) {
                    for (i in c[u][v].indices) {
                        if (f[u][v][i] < c[u][v][i]) {
                            paths[v] = Pair(u, i)
                            val addingFlow = min(flow, c[u][v][i] - f[u][v][i])
                            queue.addLast(Pair(v, addingFlow))
                            if (v == end) {
                                return addingFlow
                            }
                            break
                        }
                    }
                }
            }
        }
        return 0L
    }


    fun getMaxFlow(start: Int, end: Int) : Long {
        val paths = Array(c.size) {
            Pair(-1, -100)
        }
        var maxFlow = 0L

        while (true) {
            val addingFlow = bfs(start, end, paths)
            if (addingFlow == 0L) {
                return maxFlow
            }
            maxFlow += addingFlow
            var v = end
            while (v != start) {
                val (u, index) = paths[v]
                f[u][v][index] += addingFlow
                f[v][u][index] -= addingFlow
                v = u
            }
        }
    }



}

fun main() {
    val n = readln().toInt()
    val m = readln().toInt()
    val graph = ArrayList<HashSet<Int>>()
    val c = Array(n) {
        Array(n) {
            ArrayList<Long>()
        }
    }

    val e = ArrayList<Pair<Int, Pair<Int, Int>>>()
    repeat(n) {
        graph.add(HashSet())
    }

    repeat(m) {
        var (a, b, f) = readln().split(" ").map(String::toInt)
        a--
        b--
        graph[a].add(b)
        graph[b].add(a)
        c[a][b].add(f.toLong())
        c[b][a].add(f.toLong())
        e.add(Pair(a, Pair(b, c[a][b].size - 1)))
    }

    val edmondsKarp = EdmondsKarp(graph, c)

    val ans = edmondsKarp.getMaxFlow(0, n - 1)
    val flows = edmondsKarp.f
    println(ans)
    //println("f : ${flows.toList()}")
    for (i in 0 until m) {
        val (a, t) = e[i]
        val (b, index) = t
        println(flows[a][b][index])
    }
}

