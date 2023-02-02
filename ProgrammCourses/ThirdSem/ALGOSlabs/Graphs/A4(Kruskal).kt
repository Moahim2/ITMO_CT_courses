
import kotlin.math.sqrt



class SNM(size : Int) {

    val p = ArrayList<Int>()
    val rang = ArrayList<Int>()

    init {
        for (i in 0 until size) {
            p.add(i)
            rang.add(1)
        }
    }

    fun get(u : Int) : Int {
        if (p[u] != u) {
            p[u] = get(p[u])
        }
        return p[u]
    }
    fun union(u : Int, v : Int) {
        val u1 = get(u)
        val v1 = get(v)
        if (v1 == u1) {
            return
        }
        if (rang[u1] == rang[v1]) {
            rang[u1]++
        }
        if (rang[u1] > rang[v1]) {
            p[v1] = u1
        } else {
            p[u1] = v1
        }
    }

}

fun main() {

    class T(val u : Int, val v : Int, val r : Double) : Comparable<T> {
        override fun compareTo(other: T): Int {
            return this.r.compareTo(other.r)
        }
    }

    val n = readln().toInt()
    val graphV = Array(n) {
        Pair(0, 0)
    }
    for (i in 0 until  n) {
        val (x, y) = readln().split(" ").map { it.toInt() }
        graphV[i] = Pair(x, y)
    }
    val graphE = ArrayList<T>()
    for (i in 0 until n) {
        for (j in i until n) {
            if (j != i) {
                val u = graphV[i]
                val v = graphV[j]
                graphE.add(T(i, j, sqrt(((u.first - v.first) * (u.first - v.first) + (u.second - v.second) * (u.second - v.second)).toDouble())))
            }
        }
    }
    graphE.sort()
    val SNM = SNM(n)
    var weight = 0.0
    for (e in graphE) {
        if (SNM.get(e.u) != SNM.get(e.v)) {
            weight += e.r
            SNM.union(e.u, e.v)
        }
    }
    println(weight)
}