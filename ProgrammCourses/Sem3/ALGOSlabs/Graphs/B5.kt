import kotlin.collections.ArrayList

fun main() {
    val (n, m) = readln().split(" ").map { it.toInt() }

    val graph = Array<ArrayList<Pair<Int, Int>>>(n) {
        ArrayList()
    }

    repeat(m) {
        val line = readln().split(" ")
        val b = line[0].toInt() - 1
        val e = line[1].toInt() - 1
        val w = line[2].toInt()
        graph[b].add(Pair(e, w))
        graph[e].add(Pair(b, w))
    }

    var d = Array(0) {
        0
    }
    var pq: ArrayList<Int>

    val comporator = kotlin.Comparator<Int> { i1 : Int, i2 : Int ->
        return@Comparator if (d[i1] != d[i2]) {
            d[i1].compareTo(d[i2])
        } else {
            i1.compareTo(i2)
        }
    }

    fun deykstra(a : Int, b : Int) : Int {
        val s = 0

        d = Array(n) {
            1000000 * 2 *  n + 1000
        }

        d[s] = 0

        pq = ArrayList()
        for (i in 0 until n) {
            pq.add(i)
        }

        while (pq.isNotEmpty()) {
            val v = pq.minWith(comporator)
            pq.remove(v)
            for (all in graph[v]) {
                val u = all.first
                //println("$v $u")
                val w = if ((v == a && u == b) || (v == b && u == a)) {
                   // println("DDDDDDDDDDDDDDDDD")
                    all.second * 2
                } else {
                    all.second
                }
                if (d[u] > d[v] + w) {
                    d[u] = d[u].coerceAtMost((d[v] + w))
                }
            }
        }
        //println()
        return d[n - 1]
    }



    fun deykstra1() : Pair<Int, ArrayList<Pair<Int, Int>>> {
        val s = 0

        d = Array(n) {
            1000000 * 2 * n + 1000
        }

        val parent = Array(n) {
            -1
        }
        d[s] = 0

        pq = ArrayList()
        for (i in 0 until n) {
            pq.add(i)
        }

        while (pq.isNotEmpty()) {
            val v = pq.minWith(comporator)
            pq.remove(v)
            for (all in graph[v]) {
                val u = all.first
                val w = all.second
                if (d[u] > d[v] + w) {
                    d[u] = d[v] + w
                    parent[u] = v
                }
            }
        }
        val path = ArrayList<Pair<Int, Int>>()
        var i = n - 1
        while (i != 0) {
            val j = parent[i]
            path.add(Pair(j, i))
            i = j
        }
        return Pair(d[n - 1], path)
    }

    val d1 = deykstra1()
    //println(d1)
    var cur = 0
    for (all in d1.second) {
        val dd = deykstra(all.first, all.second)
        cur = cur.coerceAtLeast(dd - d1.first)
    }

    println(cur)

}
