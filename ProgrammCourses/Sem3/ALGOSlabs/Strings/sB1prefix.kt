fun main() {
    val subs = readln()
    val line = readln()
    val l = subs.length

    val newL = "$subs!$line"
    val p = Array(newL.length) {
        0
    }

    for (i in 1 until newL.length) {
        var s = p[i - 1]
        while (s > 0 && newL[i] != newL[s]) {
            s = p[s - 1]
        }
        if (newL[s] == newL[i]) {
            s++
        }
        p[i] = s
    }

    val ans = ArrayList<Int>()

    for (i in l+1 until newL.length) {
        if (p[i] == l) {
            ans.add(i - l * 2 + 1)
        }
    }

    println(ans.size)
    for (i in ans) {
        print("$i ")
    }
}