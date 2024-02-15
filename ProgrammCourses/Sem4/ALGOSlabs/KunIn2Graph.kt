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
        return p
    }

    fun getMaxMatchingIn1Form() : IntArray {
        return p
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

fun main() {
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
    val kunIn2Graph = KunIn2Graph(graph)
    val ans = kunIn2Graph.getMaxMatchingIn2Form(n)
    println(ans.size)
    ans.stream().forEach {
        println("${it.first} ${it.second}")
    }


}
