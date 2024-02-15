import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File

fun main() {
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


    val istream = BufferedReader(File("destroy.in").inputStream().reader())
    val input1 = istream.readLine().split(" ")
    val n = input1[0].toInt()
    val m = input1[1].toInt()
    val s = input1[2].toULong()
    val graphE = Array(m) {
        Pair(Pair(0, 0), Pair(0uL, 0))
    }
    var fullW = 0uL

    repeat(m) {
        val t = istream.readLine().split(" ")
        graphE[it] = Pair(Pair(t[0].toInt() - 1, t[1].toInt() - 1), Pair(t[2].toULong(), it))
        fullW += t[2].toULong()
    }
    istream.close()
    val comporator = kotlin.Comparator<Pair<Pair<Int, Int>, Pair<ULong, Int>>> {
            p1: Pair<Pair<Int, Int>, Pair<ULong, Int>>, p2 : Pair<Pair<Int, Int>, Pair<ULong, Int>> ->
        return@Comparator -p1.second.first.compareTo(p2.second.first)
    }

    graphE.sortWith(comporator)

    val numbersE = BooleanArray(m) {
        false
    }

    var ans = 0
    val snm = SNM(n)
    var weight = 0uL
    for (e in graphE) {
        if (snm.get(e.first.first) != snm.get(e.first.second)) {
            weight += e.second.first
            //println(weight)
            snm.union(e.first.first, e.first.second)
            numbersE[e.second.second] = true
            //println("${e.second.first} ${e.second.second}")
            ans++
        }
    }
   // println(s)
    var ind = 0
    while (fullW - weight > s && ind < m) {
        if (!numbersE[graphE[ind].second.second]) {
            weight += graphE[ind].second.first
            numbersE[graphE[ind].second.second] = true
            ans++
        }
        ind++
    }

    val out = BufferedWriter(File("destroy.out").outputStream().writer())
    out.write("${m - ans}\n")
    for (i in 0 until m) {
        if (!numbersE[i]) {
            out.write("${i + 1} ")
        }
    }
    out.close()
}
