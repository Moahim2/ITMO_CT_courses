fun main() {
    val n = readln().toInt()
    val m = readln().toInt()
    val graph = Array<HashSet<Int>>(n) {
        HashSet()
    }

    val invGraph = Array<HashSet<Int>>(n) {
        HashSet()
    }

    val vis = Array(n) {
        false
    }

    val components = Array(n) {
        -1
    }

    for (i in 0 until m) {
        val line = readln().split(" ")
        val u = line[0].toInt() - 1
        val v = line[1].toInt() - 1
        graph[u].add(v)
        invGraph[v].add(u)
    }


    val p = ArrayList<Int>()
    fun dfs1(u : Int) {
        vis[u] = true
        for (v in graph[u]) {
            if (!vis[v]) {
                dfs1(v)
            }
        }
        p.add(u)
    }
    for (i in 0 until n) {
        if (!vis[i]) {
            dfs1(i)
        }
    }

    var curComp = 0
    val graphComponents = ArrayList<HashSet<Int>>()
    fun dfs2(u : Int) {
        components[u] = curComp
        for (v in invGraph[u]) {
            if (components[v] == -1) {
                dfs2(v)
            } else if (components[v] != curComp) {
                graphComponents[components[v]].add(curComp)
            }
        }
    }
    p.reverse()
    val vOnComponent = ArrayList<Int>()
    for (v in p) {
        if (components[v] == -1) {
            graphComponents.add(HashSet())
            vOnComponent.add(v)
            dfs2(v)
            curComp++
        }
    }
/////
//    for (u in components) {
//        println(u)
//    }
    val ans = ArrayList<Int>()
    for (i in 0 until graphComponents.size) {
        if (graphComponents[i].size == 0) {
            ans.add(vOnComponent[i] + 1)
        }
    }
    println(ans.size)
    for (v in ans) {
        print("$v ")
    }
}