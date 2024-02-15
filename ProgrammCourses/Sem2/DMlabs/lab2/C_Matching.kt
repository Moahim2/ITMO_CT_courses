import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File

fun main() {
    val istream = BufferedReader(File("matching.in").inputStream().reader())
    val n = istream.readLine().toInt()
    val weights = Array(n) {
        Pair(0, 0)
    }
    val inp = istream.readLine().split(" ")
    for (i in 0 until n) {
        weights[i] = Pair(inp[i].toInt(), i + 1)
    }

    val graphE = Array<ArrayList<Int>>(n + 1) {
        ArrayList()
    }
    repeat(n) {
        val str = istream.readLine().split(" ")
        val c = str[0].toInt()
        for (i in 1..c) {
            graphE[it + 1].add(str[i].toInt())
        }
    }
    istream.close()


    val comporator = kotlin.Comparator<Pair<Int, Int>> { p1: Pair<Int, Int>, p2: Pair<Int, Int> ->
        return@Comparator -p1.first.compareTo(p2.first)
    }
    weights.sortWith(comporator)


    val fRight = IntArray(n + 1) {
        -1
    }

    val ans = IntArray(n + 1) {
        0
    }

    val visR = BooleanArray(n + 1) {
        false
    }

    fun dfs(u : Int, type : Boolean) : Boolean {
        if (type) {
            visR[u] = true
            return if (fRight[u] == -1) {
                true
            } else {
                dfs(fRight[u], false)
            }
        } else {
            var flag = false
            for (r in graphE[u]) {
                if (!visR[r] && ans[u] != r) {
                    if (dfs(r, true)) {
                        ans[u] = r
                        fRight[r] = u
                        flag = true
                        break
                    }
                }
            }
            return flag
        }
    }

    for (e in weights) {
        dfs(e.second, false)
        for (i in 1..n) {
            visR[i] = false
        }
    }
//    val comporator2 = kotlin.Comparator<Int> { p1: Int, p2: Int ->
//        return@Comparator graphE[p1].size.compareTo(graphE[p2].size)
//    }
//    left.sortWith(comporator2)



    //println(left)


    val out = BufferedWriter(File("matching.out").outputStream().writer())
    for (i in 1..n) {
        out.write("${ans[i]} ")
    }
    out.close()
}
