import java.util.Scanner
import java.util.TreeSet
import kotlin.math.min

private class FlowAndCutEK(private val g: ArrayList<HashSet<Int>>, private val c : Array<Array<ArrayList<Long>>>) {
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


    fun cutDFS(v : Int, vis : BooleanArray) {
        if (vis[v]) {
            return
        }
        vis[v] = true
        for (u in g[v]) {
            for (i in c[u][v].indices) {
                if (c[v][u][i] - f[v][u][i] > 0) {
                    cutDFS(u, vis)
                }
            }
        }
    }


    fun getMinCutSet(start: Int, end: Int) : Pair<Long, HashSet<Int>> {
        val minCut = getMaxFlow(start, end)
        val vis = BooleanArray(c.size)
        cutDFS(start, vis)
        val setOfAchievable = HashSet<Int>()
        for (i in vis.indices) {
            if (vis[i]) {
                setOfAchievable.add(i)
            }
        }
        return Pair(minCut, setOfAchievable)
    }

    class Edge(
        var a : Int,
        var b : Int,
        var index : Int,
        var c : Long,
        var number : Int
    ) : Comparable<Edge> {
        override fun compareTo(other: Edge): Int {
            return number.compareTo(other.number)
        }
    }

    fun getMinCutEdges(start: Int, end: Int, numbers : Array<Array<ArrayList<Int>>>) : Pair<Long, TreeSet<Edge>> {
        val (minCut, setOfAchievable) = getMinCutSet(start, end)
        val fullSet = HashSet<Int>()
        for (i in c.indices) {
            fullSet.add(i)
        }
        val setOfNotAchievable = fullSet.subtract(setOfAchievable)
        val setEdges = TreeSet<Edge>()

        for (u in setOfAchievable) {
            for (v in setOfNotAchievable) {
                for (i in f[u][v].indices) {
                    setEdges.add(Edge(u, v, i, f[u][v][i], numbers[u][v][i]))
                }
            }
        }

        return Pair(minCut, setEdges)
    }


}

fun main() {
    val (n, m) = readln().split(" ").map(String::toInt)
    val graph = ArrayList<HashSet<Int>>()
    val c = Array(n) {
        Array(n) {
            ArrayList<Long>()
        }
    }

    val numbers = Array<Array<ArrayList<Int>>>(n) {
        Array(n) {
            ArrayList()
        }
    }

    

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

        //e.add(Pair(a, Pair(b, c[a][b].size - 1)))
        numbers[a][b].add(it)
        numbers[b][a].add(it)
    }

    val edmondsKarp = FlowAndCutEK(graph, c)

    val (ans, setEdges) = edmondsKarp.getMinCutEdges(0, n - 1, numbers)
    println("${setEdges.size} $ans")
    setEdges.stream().forEach {
        print("${it.number + 1} ")
    }

}





