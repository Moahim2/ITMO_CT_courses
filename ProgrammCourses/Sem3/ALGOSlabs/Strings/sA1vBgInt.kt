import java.lang.Math.pow
import java.math.BigInteger


fun main() {
    val input1 = readln().split(" ")
    val n = input1[0].toInt()
    val c = input1[1].toInt()
    val line = readln().split(" ")
    val str = Array<BigInteger>(n) {
        BigInteger.ZERO
    }
    for (i in 0 until n) {
        str[i] = BigInteger(line[i])
    }
    val m = pow(2.0, 127.0).toBigDecimal().toBigInteger()
    val p = BigInteger("101111")
    val arP = ArrayList<BigInteger>()
    var cur : BigInteger = BigInteger.ONE
    for (i in 0 until n) {
        arP.add(cur)
        cur = (cur.multiply(p).mod(m))
    }

    val hashs = ArrayList<BigInteger>()
    hashs.add(str[0])
    for (i in 1 until n) {
        hashs.add((((hashs[i - 1].multiply(p).mod(m))) + str[i]).mod(m))
    }
    fun hash(hasht : ArrayList<BigInteger>, i : Int, j : Int) : BigInteger {
        if (i == 0) {
            return hasht[j]
        }
        //println("ij : $i $j")
        return (hasht[j] - ((hasht[i - 1].multiply(arP[j - i + 1]).mod(m)))).mod(m)
    }
    val reverseStr = str.reversedArray()
    val reverseHashs = ArrayList<BigInteger>()
    reverseHashs.add(reverseStr[0])
    for (i in 1 until n) {
        reverseHashs.add((((reverseHashs[i - 1].multiply(p).mod(m))) + reverseStr[i]).mod(m))
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