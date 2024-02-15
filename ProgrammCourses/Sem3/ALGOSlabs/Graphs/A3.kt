fun main() {
    val (n, m) = readln().split(" ").map { it.toInt() }
    val graph = Array(n) {
        ArrayList<Int>()
    }

    repeat(m) {
        val (i, j) = readln().split(" ").map { it.toInt() - 1 }
        graph[i].add(j)
        graph[j].add(i)
    }

    val vis = BooleanArray(n) {
        false
    }

    val tin = IntArray(n) {
        -1
    }

    val up = IntArray(n) {
        -1
    }

    var T = 0
    val graphBridgesV = Array<ArrayList<Int>>(n) {
        ArrayList()
    }
    val isBridges = BooleanArray(n) {
        false
    }

    fun dfs(v : Int, p : Int) {
        vis[v] = true
        tin[v] = T++
        up[v] = tin[v]
        for (u in graph[v]) {
            if (u == p) {
                continue
            }
            if (!vis[u]) {
                dfs(u, v)
                up[v] = minOf(up[v], up[u])
            } else {
                up[v] = minOf(up[v], tin[u])
            }
        }
        if (up[v] == tin[v]) {
            if (p != -1) {
                //println("${v + 1} ${p + 1}")
                graphBridgesV[v].add(p)
                graphBridgesV[p].add(v)
                isBridges[v] = true
                isBridges[p] = true
            }
        }
    }

    for (i in 0 until n) {
        if (!vis[i]) {
            dfs(i, -1)
        }
    }


    val visBridgeV = BooleanArray(n) {
        false
    }


    var curSize = 0L
    fun dfs2(u : Int) {
        visBridgeV[u] = true
        curSize++
        for (v in graphBridgesV[u]) {
            if (!visBridgeV[v] && isBridges[v]) {
                dfs2(v)
            }
        }
    }

    var ans = 0L
    for (u in 0 until n) {
        if (!visBridgeV[u] && isBridges[u]) {
            dfs2(u)
            ans += (curSize * (curSize - 1) / 2) - (curSize - 1)
            curSize = 0
        }
    }
//    println(sizeBridgeComponents)
//    println(visBridgeV)

    println(ans)
}