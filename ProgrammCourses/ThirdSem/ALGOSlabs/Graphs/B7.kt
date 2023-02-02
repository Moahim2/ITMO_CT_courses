import java.util.*

fun main() {
    val n = readln().toInt()
    if (n == 2) {
        println("Schtirlitz")
        println(1)
        println(2)
        return
    }
    if (n == 3) {
        println("Schtirlitz")
        println(2)
        return
    }
    fun mex(arr : TreeSet<Int>) : Int {
        var s = 0
        for (i in arr) {
            if (s == i) {
                s++
            } else {
                break
            }
        }
        return s
    }

    val g = ArrayList<Int>(n)
    repeat(n + 1) {
        g.add(0)
    }
    g[1] = 1
    g[2] = 1
    g[3] = 2
    // a1 a2 a3 | a4 a5
    //g[m] = mex {g[m - 1] g[m - 2]  i=3 U g[i]xor[g[m - i - 1]]}
    for (m in 4..n) {
        val arr = TreeSet<Int>()
        arr.add(g[m - 1])
        arr.add(g[m - 2])
        for (i in 2 until m - 2) {
            arr.add(g[i].xor(g[m - i - 1]))
        }
        g[m] = mex(arr)
    }
    //println(g)
    //println(g[2])
    println(if (g[n] == 0) {
        "Mueller"
    } else {
        "Schtirlitz"
    })
    fun mustKill(k : Int) : Boolean {
        if (k == 1 || k == n) {
            return g[n - 1] == 0
        }
        if (k == 2 || k == n - 1) {
            return g[n - 2] == 0
        }
        return (g[k - 1].xor(g[n - k])) == 0
    }

    if (g[n] != 0) {
        for (i in 1..n) {
            if (mustKill(i)) {
                println(i)
            }
        }
    }


}

