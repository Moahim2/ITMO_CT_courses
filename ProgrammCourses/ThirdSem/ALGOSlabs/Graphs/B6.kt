fun my_minim(x : Int, y : Int) : Int {
    val k = (x - y).shr(31)
    return x.and(k) + y.and(k.inv())
}

fun main() {
    val n = readln().toInt()
    val graphD = Array(n) {
        Array(n) {
            0
        }
    }

    for (i in 0 until n) {
        val s = readln().split(" ").map { it.toInt() }
        for (j in 0 until n) {
            graphD[i][j] = s[j]
        }
    }
    val range = readln().split(" ").map { it.toInt() - 1 }
    val reverseRange = range.reversed()
    //println(reverseRange)
    val ans = ArrayList<Int>()
    var k: Int
    var sum: Int
    val x = 0 until n
    for (ki in x) {
        k = reverseRange[ki]
        for (i in x) {
            for (j in x) {
                // println("$i $j $k")
                graphD[i][j] = my_minim(graphD[i][j], graphD[i][k] + graphD[k][j])
            }
        }
        sum = 0
        for (i in 0..ki) {
            for (j in 0..ki) {
                sum += graphD[reverseRange[i]][reverseRange[j]]
            }
        }
        ans.add(sum)
    }

    for (i in ans.reversed()) {
        print("$i ")
    }
}