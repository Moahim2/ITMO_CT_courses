
fun main() {
    val subs = readln()
    val l = subs.length
    val line = readln()
    val n = line.length

    //val m = pow(2.0, 32.0).toBigDecimal().toBigInteger()
    val p : ULong = 2017u

    val arP1 = ArrayList<ULong>()
    var cur : ULong = 1u
    for (i in 0 until n) {
        arP1.add(cur)
        cur = (cur* p)
    }

    val hashs = ArrayList<ULong>()
    hashs.add(line[0].code.toULong())
    for (i in 1 until n) {
        hashs.add((((hashs[i - 1] * p)) + line[i].code.toULong()))
    }

    val p2 : ULong = 2003u


    val arP2 = ArrayList<ULong>()
    var cur2 : ULong = 1u
    for (i in 0 until n) {
        arP2.add(cur)
        cur2 = (cur2 * p)
    }

    val hashs2 = ArrayList<ULong>()
    hashs2.add(line[0].code.toULong())
    for (i in 1 until n) {
        hashs2.add((((hashs2[i - 1] * p)) + line[i].code.toULong()))
    }


    val subsHashs = ArrayList<ULong>()
    subsHashs.add(subs[0].code.toULong())
    for (i in 1 until l) {
        subsHashs.add((((subsHashs[i - 1] * p)) + subs[i].code.toULong()))
    }
    //println(subsHashs)
    val h = subsHashs.last()
    fun hash(hashr : ArrayList<ULong>, ar : ArrayList<ULong>, i : Int, j : Int) : ULong {
        if (i == 0) {
            return hashr[j]
        }
        //println("ij : $i $j")
        return (hashr[j] - ((hashr[i - 1] * (ar[j - i + 1]))))
    }

    val ans = ArrayList<Int>()
    for (i in 0..n-l) {
        if (hash(hashs, arP1, i, i + l - 1) == h) {
            if (line[i] == subs.first() && line[i + l - 1] == subs.last()) {
                ans.add(i + 1)
            }
            //ans.add(i + 1)
        }
    }

    println(ans.size)
    for (i in ans) {
        print("$i ")
    }
}
