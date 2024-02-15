import java.lang.IllegalArgumentException

private class FindMax2Subgraph(private val graph: ArrayList<ArrayList<Int>>, private val n : Int) {
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

        fun getMaxMatchingIn2Form(n : Int) : ArrayList<Pair<Int, Int>> {
            //n - leftSize
            p = getMaxMatching()
            val ans = ArrayList<Pair<Int, Int>>()

            for (i in 0 until n) {
                if (p[i] != -1) {
                    ans.add(Pair(i + 1, p[i] - n + 1))
                }
            }
            return ans
        }

    }

    private fun getAntiGraph() : ArrayList<ArrayList<Int>> {
        val antiGraph = ArrayList<ArrayList<Int>>()

        for (i in graph.indices) {
            antiGraph.add(ArrayList())
        }

        for (i in 0 until n) {
            for (j in n until graph.size) {
                if (!graph[i].contains(j)) {
                    antiGraph[i].add(j)
                    antiGraph[j].add(i)
                }
            }
        }
        return antiGraph
    }

    fun getMinVertexCover(g : ArrayList<ArrayList<Int>>, cn : Int = n) : Pair<List<Int>, List<Int>> {
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
                    if (p[v] == u && p[u] != v) {
                        throw IllegalArgumentException()
                    }
                    if (p[u] != v && p[v] != u) { //не из парсоча
                        dfs(v)
                    }
                }
            }
        }

        for (u in 0 until n) {
            if (p[u] == -1) {
                dfs(u)
            }
        }
        val l1 = ArrayList<Int>()
        val r1 = ArrayList<Int>()

        for (i in 0 until n) {
            if (!vis[i]) {
                l1.add(i)
            }
        }

        for (i in n until g.size) {
            if (vis[i]) {
                r1.add(i)
            }
        }

        return Pair(l1.sorted(), r1.sorted())
    }

    fun getMax2Subgraph() : Pair<List<Int>, List<Int>> {
        val g = getAntiGraph()
        val (l, r) = getMinVertexCover(g)
       // println(g)
        //println("$l xxx $r")
        val l1 = ArrayList<Int>()
        val r1 = ArrayList<Int>()

        for (i in 0 until n) {
            if (!l.contains(i)) {
                l1.add(i + 1)
            }
        }

        for (i in n until g.size) {
            if (!r.contains(i)) {
                r1.add(i - n + 1)
            }
        }

        return Pair(l1.sorted(), r1.sorted())
    }
}

fun main() {
    val kk = readln().toInt()


    repeat(kk) {
        val (n, m) = readln().split(" ").map(String::toInt)
        val graph = ArrayList<ArrayList<Int>>()
        for (i in 0 until (n+m)) {
            graph.add(ArrayList())
        }
        for (i in 0 until n) {
            val numbers = readln().split(" ").map(String::toInt)
            for (j in 0 until numbers.size - 1) {
                val k = numbers[j] + n - 1
                graph[i].add(k)
                graph[k].add(i)
            }
        }

        val findMax2Subgraph = FindMax2Subgraph(graph, n)
        val (l, r) = findMax2Subgraph.getMax2Subgraph()
        println(l.size + r.size)
        println("${l.size} ${r.size}")
        l.stream().forEach {
            print("$it ")
        }
        println()
        r.stream().forEach {
            print("$it ")
        }


    }

}