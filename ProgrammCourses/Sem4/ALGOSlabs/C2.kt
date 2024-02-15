import java.util.*
import kotlin.collections.ArrayDeque
import kotlin.collections.HashMap
import kotlin.collections.HashSet
import kotlin.math.min

private class C2(private val g: Array<HashSet<Int>>, private val c : Array<LongArray>) {
    val f = Array(c.size) {
        LongArray(c.size)
    }


    private fun bfs(start: Int, end : Int, paths : IntArray) : Long {
        for (i in paths.indices) {
            paths[i] = -1
        }
        paths[start] = -2
        val queue = ArrayDeque<Pair<Int, Long>>()
        queue.addLast(Pair(start, Long.MAX_VALUE))

        while (queue.isNotEmpty()) {
            val (u, flow) = queue.first()
            queue.removeFirst()
            for (v in g[u]) {
                if (paths[v] == -1) {
                    if (f[u][v] < c[u][v]) {
                        paths[v] = u
                        val addingFlow = min(flow, c[u][v] - f[u][v])
                        queue.addLast(Pair(v, addingFlow))
                        if (v == end) {
                            return addingFlow
                        }

                    }
                }
            }
        }
        return 0L
    }


    fun getMaxFlow(start: Int, end: Int) : Long {
        val paths = IntArray(c.size) {
            -1
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
                val u = paths[v]
                f[u][v] += addingFlow
                f[v][u] -= addingFlow
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
            if (c[v][u] - f[v][u] > 0) {
                cutDFS(u, vis)
            }
        }
    }


    fun getMinCutSet(start: Int, end: Int) : Pair<Long, HashSet<Int>> {
        val minCut = getMaxFlow(start, end)
        val vis = BooleanArray(c.size)
        cutDFS(start, vis)
        val setOfAchievable = HashSet<Int>()

        /////////////////!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if (minCut >= Int.MAX_VALUE) {
            return Pair(minCut, setOfAchievable)
        }
        ///
        ///
        for (i in vis.indices) {
            if (vis[i]) {
                setOfAchievable.add(i)
            }
        }
        return Pair(minCut, setOfAchievable)
    }

    fun getMinCutEdges(start: Int, end: Int, numbers : Array<IntArray>) : Pair<Long, Set<Int>> {
        val (minCut, setOfAchievable) = getMinCutSet(start, end)

        //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        //
        //
        if (minCut >= Int.MAX_VALUE) {
            return Pair(minCut, TreeSet())
        }
        //
        //
        //

        val fullSet = HashSet<Int>()
        for (i in c.indices) {
            fullSet.add(i)
        }
        val setOfNotAchievable = fullSet.subtract(setOfAchievable)
        val setEdges = HashSet<Int>()

        for (u in setOfAchievable) {
            for (v in setOfNotAchievable) {
                    ////////!!!!!!!!!!!!!!!!!!!!!!!!!
                if (c[u][v] != 0L) {
                    setEdges.add(numbers[u][v])
                }
            }
        }

        return Pair(minCut, setEdges)
    }


}


fun main() {
    val (m, n) = readln().split(" ").map(String::toInt)

    val map = Array(m) {
        CharArray(n)
    }

    repeat(m) {
        val str = readln()
        map[it] = str.toCharArray()
    }

    val count = 2 * n * m

    val graph = Array<HashSet<Int>>(count) {
        HashSet()
    }

    val c = Array(count) {
        LongArray(count)
    }

    val numbers = Array(count) {
        IntArray(count)
    }


    val shift = m * n

    fun getNM(p : Int) : Pair<Int, Int> {
        return Pair(p / n, p % n)
    }

    val biection = HashMap<Int, Int>()
    var number = 0

    var start = -1
    var end  = -2

    for (i in map.indices) {
        for (j in map[i].indices) {
            val cur = i * n + j

            if (map[i][j] == '#') {
                continue
            }

            if (map[i][j] == 'A') {
                start = cur
            }

            if (map[i][j] == 'B') {
                end = cur
            }

            graph[cur].add(cur + shift)
            graph[cur + shift].add(cur)
            if (map[i][j] == '-' || map[i][j] == 'A' || map[i][j] == 'B') {
                c[cur][cur + shift] = (Int.MAX_VALUE.toLong())
            } else {
                c[cur][cur + shift] = (1)
            }
            c[cur + shift][cur] = (0L)

            numbers[cur][cur + shift] = (number)
            numbers[cur + shift][cur] = (number)
            biection[number++] = cur

            if (cur % n != 0) {
                val d = cur - 1

                val (a, b) = getNM(d)
                if (map[a][b] != '#') {
                    graph[cur + shift].add(d)
                    graph[d].add(cur + shift)
                    c[cur + shift][d] = (Int.MAX_VALUE.toLong())
                    c[d][cur + shift] = (0)

                    graph[d + shift].add(cur)
                    graph[cur].add(d + shift)
                    c[d + shift][cur] = (Int.MAX_VALUE.toLong())
                    c[cur][d + shift] = (0)
                }
            }
            if ((cur + 1) % n != 0) {
                val d = cur + 1

                val (a, b) = getNM(d)
                if (map[a][b] != '#') {
                    graph[cur + shift].add(d)
                    graph[d].add(cur + shift)
                    c[cur + shift][d] = (Int.MAX_VALUE.toLong())
                    c[d][cur + shift] = (0)

                    graph[d + shift].add(cur)
                    graph[cur].add(d + shift)
                    c[d + shift][cur] = (Int.MAX_VALUE.toLong())
                    c[cur][d + shift] = (0)
                }
            }
            if (cur + n < shift) {
                val d = cur + n

                val (a, b) = getNM(d)
                if (map[a][b] != '#') {
                    graph[cur + shift].add(d)
                    graph[d].add(cur + shift)
                    c[cur + shift][d] = (Int.MAX_VALUE.toLong())
                    c[d][cur + shift] = (0)

                    graph[d + shift].add(cur)
                    graph[cur].add(d + shift)
                    c[d + shift][cur] = (Int.MAX_VALUE.toLong())
                    c[cur][d + shift] = (0)
                }
            }
            if (cur - n >= 0) {
                val d = cur - n

                val (a, b) = getNM(d)
                if (map[a][b] != '#') {
                    graph[cur + shift].add(d)
                    graph[d].add(cur + shift)
                    c[cur + shift][d] = (Int.MAX_VALUE.toLong())
                    c[d][cur + shift] = (0)

                    graph[d + shift].add(cur)
                    graph[cur].add(d + shift)
                    c[d + shift][cur] = (Int.MAX_VALUE.toLong())
                    c[cur][d + shift] = (0)
                }
            }

        }
    }


    val c2 = C2(graph, c)

    val (ans, setEdges) = c2.getMinCutEdges(start, end, numbers)
    if (ans >= Int.MAX_VALUE) {
        println(-1)
        return
    }

    println(ans)

    //val set = setEdges.stream().map(C2.Edge::number).map(biection::get).toList().toHashSet()
    val set = HashSet<Int>()
    for (i in setEdges) {
        set.add(biection[i]!!)
    }

    for (i in map.indices) {
        for (j in map[i].indices) {
            if (map[i][j] != '.') {
                print(map[i][j])
                continue
            }
            val cur = i * n + j
            if (set.contains(cur)) {
                print('+')
            } else {
                print('.')
            }
        }
        println()
    }


}
