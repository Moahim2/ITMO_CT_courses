import APM.Companion.parseCombObjects
import APM.Companion.toPString
import kotlin.math.min

class APM {
    // A = 
    companion object {

        private fun getReversedOnMod(a : Long, mod: Long) : Long {
            return (reversedOnMod(a, mod).first.mod(mod) + mod).mod(mod)
        }

        private fun reversedOnMod(a : Long, mod : Long) : Pair<Long, Long> {
            if (a == 0L) {
                return Pair(0, 1)
            }
            val (x, y) = reversedOnMod(mod.mod(a), a)
            return Pair(y - (mod / a) * x, x)
        }

        fun List<Long>.getI(i: Int): Long {
            return if (this.size > i) {
                this[i]
            } else {
                0
            }
        }
        fun LongArray.getI(i: Int): Long {
            return if (this.size > i) {
                this[i]
            } else {
                0
            }
        }

        private fun polynomialWithoutHeadZeroes(P : List<Long>) : List<Long> {
            val PR = P.reversed()
            val Q = ArrayList<Long>()
            var flag = false
            for (pr in PR) {
                if (flag) {
                    Q.add(pr)
                } else {
                    if (pr != 0L) {
                        flag = true
                        Q.add(pr)
                    }
                }
            }
            return Q.reversed()
        }

        private fun sum(P: List<Long>, Q: List<Long>, mod: Long = Long.MAX_VALUE, cropLength: Int = -1): List<Long> {
            val S = ArrayList<Long>()
            for (i in 0 until maxOf(P.size, Q.size)) {
                S.add(
                        if (mod != -1L) {
                            (P.getI(i) + Q.getI(i)).mod(mod)
                        } else {
                            (P.getI(i) + Q.getI(i))
                        }
                )
            }
//            val R = polynomialWithoutHeadZeroes(S)
//            if (R.isEmpty()) { //000000000000
//                return listOf(0L)
//            }
            return if (cropLength == -1) {
                S
            } else {
                S.subList(0, minOf(cropLength, S.size)).toMutableList()
            }
        }

        private fun sub(P: List<Long>, Q: List<Long>, mod: Long = -1L, cropLength: Int = -1): List<Long> {
            val S = ArrayList<Long>()
            for (i in 0 until maxOf(P.size, Q.size)) {
                S.add(
                        if (mod != -1L) {
                            (P.getI(i) - Q.getI(i)).mod(mod)
                        } else {
                            (P.getI(i) - Q.getI(i))
                        }
                )
            }
//            val R = polynomialWithoutHeadZeroes(S)
//            if (R.isEmpty()) { //000000000000
//                return listOf(0L)
//            }
            return if (cropLength == -1) {
                S
            } else {
                S.subList(0, minOf(cropLength, S.size)).toMutableList()
            }
        }

        private fun mul(P: List<Long>, Q: List<Long>, mod: Long = -1L, cropLength: Int = -1): List<Long> {
            if (P.isEmpty() || Q.isEmpty() || (P.size == 1 && P[0] == 0L) || (Q.size == 1 && Q[0] == 0L)) { //константный ноль
                return listOf(0L)
            }

            val S = LongArray(P.size + Q.size - 1)
            for (n in 0 until P.size + Q.size - 1) {
                var s = 0L
                for (i in 0..n) {
                    s =
                            if (mod != -1L) {
                        (s + (P.getI(i) * Q.getI(n - i)).mod(mod)).mod(mod)
                    } else {
                        s + (P.getI(i) * Q.getI(n - i))
                    }
                }
                S[n] = s
            }
            //println("$P, $Q")
//            val R = polynomialWithoutHeadZeroes(S)
//            if (R.isEmpty()) {
//                return listOf(0L)
//            }
            return if (cropLength == -1) { //S->R
                S.toList()
            } else {
                S.toList().subList(0, minOf(cropLength, S.size))
            }
        }

        private fun mulArrEqualsN(P: LongArray, Q: LongArray, mod: Long = Long.MAX_VALUE): LongArray {
            val k = P.size
            val S = LongArray(k + k - 1)
            for (n in 0 until k + k - 1) {
                var s = 0L
                if (n < k) {
                    for (i in 0..n) {
                        s = (s + (P[i] * Q[n - i]).mod(mod)).mod(mod)
                    }
                } else {
                    for (i in (n - k + 1) until k) {
                        s = (s + (P[i] * Q[n - i]).mod(mod)).mod(mod)
                    }
                }
                S[n] = s
            }
//            val R = polynomialWithoutHeadZeroes(S)
//            if (R.isEmpty()) {
//                return listOf(0L)
//            }
            return S
        }

        fun div(P: List<Long>, Q: List<Long>, mod: Long = Long.MAX_VALUE, count : Int): List<Long> {
            val S = ArrayList<Long>()
//            val r = (reversedOnMod(Q[0], mod).first.mod(mod) + mod).mod(mod)
            //q[0] == 1 - всегда((((

            for (n in 0 until count) {
                var sum = 0L
                for (i in 0 until n) {
                    sum = if (mod == -1L) {
                        (sum + (S.getI(i) * Q.getI(n - i)))
                    } else {
                        (sum + (S.getI(i) * Q.getI(n - i)).mod(mod)).mod(mod)
                    }
                }
                if (mod == -1L) {
                    S.add((P.getI(n) - sum))
                } else {
                    S.add((P.getI(n) - sum).mod(mod))
                }
            }
            return S
        }

        fun List<Long>.toPString() : String {
            if (this.isEmpty()) {
                return "0"
            }
            return this.joinToString(" ")
        }

        fun sqrt1plusPT(P: List<Long>, mod: Long, m: Int) : List<Long> {
            var ans = listOf(0L)
            var PN = listOf(1L)
            var factorialAndTwo = 1L
            var d: Long
            var g = 1L
            for (i in 0..m) {
                ans = sum(ans, mul(PN, listOf((getReversedOnMod(factorialAndTwo, mod) * g).mod(mod)), mod, m), mod, m)

                PN = mul(PN, P, mod, m)
                d = (1L - i * 2).mod(mod)
                g = (g * d).mod(mod)
                factorialAndTwo = (factorialAndTwo * (i + 1) * 2).mod(mod)
            }
            val ansWithZeroes = ArrayList(ans)
            for (i in ans.size..m) {
                ansWithZeroes.add(0L)
            }
            return ansWithZeroes.subList(0, m).toMutableList()
        }

        fun exp(P: List<Long>, mod: Long, m : Int) : List<Long> {
            var ans = listOf(0L)
            var PN = listOf(1L)
            var factorial = 1L
            for (i in 0..m) {
                ans = sum(ans, mul(PN, listOf(getReversedOnMod(factorial, mod)), mod, m), mod, m)
                PN = mul(PN, P, mod, m)
                factorial = (factorial * (i + 1)).mod(mod)
            }
            val ansWithZeroes = ArrayList(ans)
            for (i in ans.size..m) {
                ansWithZeroes.add(0L)
            }
            return ansWithZeroes.subList(0, m).toMutableList()
        }

        fun log(P: List<Long>, mod: Long, m : Int) : List<Long> {
            var ans = listOf(0L)
            var PN = P
            var d = 1L
            for (i in 1..m) {
                ans = if (i % 2 == 1) {
                    sum(ans, mul(PN, listOf(getReversedOnMod(d, mod)), mod, m), mod, m)
                } else {
                    sub(ans, mul(PN, listOf(getReversedOnMod(d, mod)), mod, m), mod, m)
                }
                PN = mul(PN, P, mod, m)
                d = (d + 1L).mod(mod)
            }
            val ansWithZeroes = ArrayList(ans)
            for (i in ans.size..m) {
                ansWithZeroes.add(0L)
            }
            return ansWithZeroes.subList(0, m).toMutableList()
        }

        private fun shiftToRightAndAdd(A: List<Long>, e : Long = 0) : List<Long> {
            val list = ArrayList<Long>()
            list.add(e)
            list.addAll(A)
            return list
        }

        private fun getPQOnRecompetitive(A : List<Long>, C : List<Long>) : Pair<List<Long>, List<Long>> {
            val Q = sub(listOf(0L), shiftToRightAndAdd(C, -1L), -1L)
            val P = ArrayList<Long>()
            val AA = ArrayList<Long>()
            for (i in A.indices) {
                AA.add(0)
            }
            AA.addAll(A)
            for (i in 0..Q.size - 2) {
                //p_i = a_i - a_i-1 * c_1 - ... a_i -j * c_j
                var sum = 0L
                for (j in A.indices) {
                    sum += AA[A.size + i - j - 1] * C[j]
                }
                P.add(A[i] - sum)
            }
            return Pair(polynomialWithoutHeadZeroes(P), Q)
        }

        private fun getValI(P : List<Long>, cur : Long) : Long {
            var s = 0L
            var c = 1L
            for (i in P.indices) {
                s = s + c * P[i]
                c = c * cur
            }
            return s
        }

        fun getPQon1KvaziPolynom(r : Int, F : List<Long>) : Pair<List<Long>, List<Long>> {
            var Q = listOf(1L)
            val R = listOf(1L, -(r.toLong()))
            for (i in F.indices) {
                Q = mul(Q, R, -1L)
            }
            val C = ArrayList<Long>()
            for (i in 1 until Q.size) {
                C.add(-Q[i])
            }
            val A = ArrayList<Long>()
            var rn = 1L
            for (i in 0..Q.size - 2) {
                A.add(rn * getValI(F, i.toLong()))
                rn = rn * r
            }
            return Pair(getPQOnRecompetitive(A, C).first, Q)
        }

        fun getAlternatePolynomial(P : List<Long>, mod: Long) : List<Long> {
            val AP = ArrayList<Long>()
            for (i in P.indices) {
                if (i % 2 == 1) {
                    AP.add((-P[i]).mod(mod))
                } else {
                    AP.add(P[i])
                }
            }
            return AP
        }

        fun getAlternatePolynomialArr(P : LongArray, mod: Long) : LongArray {
            val AP = LongArray(P.size)
            for (i in P.indices) {
                if (i % 2 == 1) {
                    AP[i] = ((-P[i]).mod(mod))
                } else {
                    AP[i] = P[i]
                }
            }
            return AP
        }

        fun getArrayAlternatePolynomial(P : LongArray, mod: Long) : LongArray {
            val AP = LongArray(P.size)
            for (i in P.indices) {
                if (i % 2 == 1) {
                    AP[i] = ((-P[i]).mod(mod))
                } else {
                    AP[i] = (P[i])
                }
            }
            return AP
        }

        fun fastPow(P : List<Long>, n: Long, mod : Long, cropLength: Int) : List<Long> {
            if (mod != -1L) {
                return listOf()//доделать
            }

            if (n == 0L) {
                return listOf(1L)
            }
            return if (n % 2 == 0L) {
                val x = fastPow(P, n / 2, mod, cropLength)
                mul(x, x, mod, cropLength)
            } else {
                mul(fastPow(P, n - 1, mod, cropLength), P, mod, cropLength)
            }

        }

        fun getAnOnRecompetitive(Az : List<Long>, Cz : List<Long>, number : Long, mod : Long) : Long {
            var C = ArrayList(polynomialWithoutHeadZeroes(Cz))
            var A = ArrayList(Az.subList(0, C.size))
            var Q = sub(listOf(1L), mul(C, listOf(0L, 1L), mod), mod)
            for (i in A.indices) {
                A.add(0L)
            }
            var n = number
            while (n >= C.size) {
                //println(C)
                //println(A)
                for (i in C.size until A.size) { //пересчет ашек
                    var sum = 0L
                    for (j in 1 until Q.size) {
                        sum = (sum - (A[i - j] * Q[j]).mod(mod)).mod(mod)
                    }
                    A[i] = sum.mod(mod)
                }

                val flag = n.mod(2)
                n /= 2

                val D = ArrayList<Long>()
                for (i in A.indices) {
                    if ((i.mod(2)) == flag) {
                        D.add(A[i])
                    }
                }

                Q = mul(Q, getAlternatePolynomial(Q, mod), mod)
                val X = LongArray(Q.size / 2 + Q.size % 2)
                for (i in Q.indices) {
                    if (i % 2 == 0) {
                        X[i / 2] = Q[i]
                    }
                }
                Q = X.toList()

                A = D
                for (i in A.size until C.size * 2) {
                    A.add(0L)
                }
            }
           // println(A)
            return A[n.toInt()]
        }


        fun L(X : List<Long>, cropLength : Int) : List<Long> {
            return div(
                        listOf(1L),
                        sub(
                                listOf(1L),
                                X,
                                -1L,
                                cropLength
                        ),
                        -1L,
                        cropLength
            )
        }

        fun P(X : List<Long>, Y : List<Long>, cropLength: Int) : List<Long> {
            return mul(X, Y, -1L, cropLength)
        }

        fun S(X : List<Long>, cropLength: Int) : List<Long> {
            var mul = listOf(1L)
            for (i in 1 until minOf(cropLength, X.size)) {
                val ti = ArrayList<Long>()
                for (j in 0..i) {
                    if (j == i) {
                        ti.add(1L)
                    } else {
                        ti.add(0L)
                    }
                }
                mul = mul(
                        mul,
                        fastPow(
                                div(
                                        listOf(1L),
                                        sub(
                                                listOf(1L),
                                                ti,
                                                -1L,
                                                cropLength
                                        ),
                                        -1L,
                                        cropLength
                                ),
                                X[i],
                                -1L,
                                cropLength
                        ),
                        -1L,
                        cropLength
                )
            }
            return mul
        }

        fun parseCombObjects(str : String, cropLength: Int) : List<Long> {
           // println(str)
            return when (str[0]) {
                'B' -> listOf(0L, 1L)
                'P' -> {
                    var i = 1
                    var countP = -1
                    var countT = 0
                    while (str[i] != ',' || countP >= countT) {
                        if (str[i] == ',') {
                            countT++
                        }
                        if (str[i] == 'P') {
                            countP++
                        }
                        i++
                    }
                    P(
                            parseCombObjects(str.substring(2, i), cropLength),
                            parseCombObjects(str.substring(i + 1, str.length - 1), cropLength),
                            cropLength
                    )
                }
                'S' -> S(parseCombObjects(str.substring(2, str.length - 1), cropLength), cropLength)
                'L' -> L(parseCombObjects(str.substring(2, str.length - 1), cropLength), cropLength)
                else -> listOf(0L)
            }
        }

    }

    var listWeights : ArrayList<Int> = arrayListOf()
    var map : LongArray = LongArray(0)
    var mapPair : LongArray = LongArray(0)

    fun countTree(size : Int, mod: Long) : Long {
        if (map[size] != -1L) {
            return map[size]
        }
        var sum = 0L
        for (e in listWeights) {
            if (size - e < 0) {
                continue
            }
            if (mapPair[size - e] != -1L) {
                sum = (sum + mapPair[size - e]).mod(mod)
                continue
            }
            var mul = 0L
            for (i in 0..size-e) {
                val leftC = countTree(i, mod)
                map[i] = leftC
                val rightC = countTree(size - e - i, mod)
                map[size - e - i] = rightC
                mul = (mul + (leftC * rightC).mod(mod)).mod(mod)
            }
            mapPair[size - e] = mul
            sum = (sum + mul).mod(mod)
        }
        map[size] = sum
        return sum
    }
}


fun main() {
    val mod = 1000000007L
    val (k, m) = readln().split(" ").map(String::toInt)
    val list = readln().split(" ").map(String::toInt).sorted()
    val apm = APM()
    apm.listWeights = ArrayList(list)
    val arr = LongArray(m + 1) {
        -1L
    }
    arr[0] = 1L
    apm.map = arr
    apm.mapPair = arr.copyOf()
    for (i in 1..m) {
        print("${apm.countTree(i, mod)} ")
    }
}
