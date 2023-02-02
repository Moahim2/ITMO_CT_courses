

fun main() {
    val input1 = readln().split(" ")
    val n = input1[0].toInt()
    val c = input1[1].toInt()
    val line = readln().split(" ")
    val str = Array<Long>(n) {
        0
    }
    for (i in 0 until n) {
        str[i] = line[i].toLong()
    }

    val arP = ArrayList<Long>()
    var cur : Long = 1
    for (i in 0 until n) {
        arP.add(cur)
        cur = (cur * 101111L)
    }
    val hashs = ArrayList<Long>()
    hashs.add(str[0])
    for (i in 1 until n) {
        hashs.add((((hashs[i - 1] * 101111L)) + str[i]))
    }
    fun hash(hasht : ArrayList<Long>, i : Int, j : Int) : Long {
        if (i == 0) {
            return hasht[j]
        }
        //println("ij : $i $j")
            return (hasht[j] - ((hasht[i - 1] * arP[j - i + 1])))
    }
    val reverseStr = str.reversedArray()
    val reverseHashs = ArrayList<Long>()
    reverseHashs.add(reverseStr[0])
    for (i in 1 until n) {
        reverseHashs.add((((reverseHashs[i - 1] * 101111L)) + reverseStr[i]))
    }

    val ans = ArrayList<Int>()


//    println(hashs)
//    println(reverseHashs)
    ans.add(n)
    for (i in 1..n/2) {
        val left = hashs[i - 1]
        val right = hash(reverseHashs, n - 2 * i, n - i - 1)
        //println("t : $left $right")
        if (left == right) {
            ans.add(n - i)
        }
    }
    for (i in ans) {
        print("$i ")
    }
}