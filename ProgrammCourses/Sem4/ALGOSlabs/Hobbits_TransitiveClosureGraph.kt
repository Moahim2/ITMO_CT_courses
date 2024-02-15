import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.FileReader
import java.io.FileWriter


class Hobbits_TransitiveClosureGraph(private val orGraph : ArrayList<ArrayList<Int>>) {
    private class KunIn2Graph(private val graph : ArrayList<ArrayList<Int>>) {
        private var vis : BooleanArray = BooleanArray(graph.size)
        private var p : IntArray = IntArray(graph.size) {
            -1
        }

        private fun dfs(v : Int) : Boolean {
            if (vis[v]) {
                return false
            }
            vis[v] = true
            for (u in graph[v]) {
                if (p[u] == -1 || dfs(p[u])) {
                    p[u] = v
                    return true
                }
            }
            return false
        }

        private fun getMaxMatching() : IntArray {
            for (v in graph.indices) {
                for (j in vis.indices) {
                    vis[j] = false
                }
                dfs(v)
            }
            for (i in p.indices) {
                if (p[i] != -1) {
                    p[p[i]] = i
                }
            }
            return p
        }

        fun getMaxMatchingIn1Form() : IntArray {
            return getMaxMatching()
        }

    }

    fun getMinVertexCover(g : ArrayList<ArrayList<Int>>, cn : Int) : Pair<List<Int>, List<Int>> {
        val kunIn2Graph = KunIn2Graph(g)
        val p = kunIn2Graph.getMaxMatchingIn1Form()
        //println("P :${p.toList()}")
        val vis = BooleanArray(g.size)

        fun dfs(u : Int) {
            if (vis[u]) {
                return
            }
            vis[u] = true
            if (u >= cn) {//правые
                if (p[u] != -1) { //из парсоча
                    dfs(p[u])
                }
            } else {
                for (v in g[u]) { //левые
                    if (p[u] != v && p[v] != u) { //не из парсоча
                        dfs(v)
                    }
                }
            }
        }

        for (u in 0 until cn) {
            if (p[u] == -1) {
                dfs(u)
            }
        }
        val l1 = ArrayList<Int>()
        val r1 = ArrayList<Int>()

        for (i in 0 until cn) {
            if (!vis[i]) {
                l1.add(i)
            }
        }

        for (i in cn until g.size) {
            if (vis[i]) {
                r1.add(i)
            }
        }

        return Pair(l1.sorted(), r1.sorted())
    }

    init { //строим замыкание
        val vis = BooleanArray(orGraph.size)
        fun transitiveDfs(u : Int) {
            if (vis[u]) {
                return
            }
            vis[u] = true
            val set = HashSet<Int>(orGraph[u])
            for (vi in orGraph[u].indices) { //
                transitiveDfs(orGraph[u][vi])
                for (childrenV in orGraph[orGraph[u][vi]].indices) {
                    if (!set.contains(orGraph[orGraph[u][vi]][childrenV])) {
                        orGraph[u].add(orGraph[orGraph[u][vi]][childrenV])
                        set.add(orGraph[orGraph[u][vi]][childrenV])
                    }
                }
            }
        }
        for (i in orGraph.indices) {
            if (!vis[i]) {
                transitiveDfs(i)
            }
        }
    }

    fun get2GraphOnOrGraph(g: ArrayList<ArrayList<Int>> = orGraph)  : ArrayList<ArrayList<Int>> { //раздваиваем вершинки
        val graph2d = ArrayList<ArrayList<Int>>()
        repeat(g.size * 2) {
            graph2d.add(ArrayList())
        }

        for (i in g.indices) {
                for (j in g[i]) {
                val r = g.size + j
                graph2d[i].add(r)
                graph2d[r].add(i)
            }
        }

        return graph2d
    }



    fun getMaxSetIncomparable() : ArrayList<Int> {
        val graph = get2GraphOnOrGraph()
        val (l1, r1) = getMinVertexCover(graph, orGraph.size)

        val ans = ArrayList<Int>()
        for (i in orGraph.indices) {
            if (!l1.contains(i) && !r1.contains(i + orGraph.size)) {
                ans.add(i)
            }
        }

        return ans
    }

}

fun main() {
    val input = BufferedReader(FileReader("hobbits.in"))
    val n = input.readLine().toInt()
    val orGraph = ArrayList<ArrayList<Int>>()

    repeat(n) {
        orGraph.add(ArrayList())
    }

    for (i in 0 until n) {
        val numbers = input.readLine().split(" ").map(String::toInt)
        for (j in 0 until n) {
            if (numbers[j] == 1) {
                orGraph[i].add(j)
            }
        }
    }
    input.close()

    val hobbitsTransitiveClosureGraph = Hobbits_TransitiveClosureGraph(orGraph)
    val ans = hobbitsTransitiveClosureGraph.getMaxSetIncomparable()

    val out = BufferedWriter(FileWriter("hobbits.out"))
    out.write(ans.size.toString())
    out.newLine()
    ans.stream().forEach {
        out.write("${it + 1} ")
    }
    out.close()
}

