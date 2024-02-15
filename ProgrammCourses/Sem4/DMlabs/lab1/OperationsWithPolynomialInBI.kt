import java.math.BigInteger
import APM1.Companion.getKvaziPolynomOnPr

class APM1 {
    companion object {

        fun getReversedOnMod(a : BigInteger, mod: BigInteger) : BigInteger {
            return (reversedOnMod(a, mod).first.mod(mod) + mod).mod(mod)
        }

        fun reversedOnMod(a : BigInteger, mod : BigInteger) : Pair<BigInteger, BigInteger> {
            if (a == BigInteger.ZERO) {
                return Pair(BigInteger.ZERO, BigInteger.ONE)
            }
            val (x, y) = reversedOnMod(mod.mod(a), a)
            return Pair(y - (mod / a) * x, x)
        }

        fun List<BigInteger>.getI(i: Int): BigInteger {
            return if (this.size > i) {
                this[i]
            } else {
                BigInteger.ZERO
            }
        }

        fun polynomialWithoutHeadZeroes(P : List<BigInteger>) : List<BigInteger> {
            val PR = P.reversed()
            val Q = ArrayList<BigInteger>()
            var flag = false
            for (pr in PR) {
                if (flag) {
                    Q.add(pr)
                } else {
                    if (pr != BigInteger.ZERO) {
                        flag = true
                        Q.add(pr)
                    }
                }
            }
            return Q.reversed()
        }

        fun sum(P: List<BigInteger>, Q: List<BigInteger>, cropLength: Int = -1): List<BigInteger> {
            val S = ArrayList<BigInteger>()
            for (i in 0 until maxOf(P.size, Q.size)) {
                S.add((P.getI(i) + Q.getI(i)))
            }
            val R = polynomialWithoutHeadZeroes(S)
            if (R.isEmpty()) { //000000000000
                return listOf(BigInteger.ZERO)
            }
            return if (cropLength == -1) {
                R
            } else {
                R.subList(0, minOf(cropLength, R.size)).toMutableList()
            }
        }

        fun sub(P: List<BigInteger>, Q: List<BigInteger>, cropLength: Int = -1): List<BigInteger> {
            val S = ArrayList<BigInteger>()
            for (i in 0 until maxOf(P.size, Q.size)) {
                S.add((P.getI(i) - Q.getI(i)))
            }
//            val R = polynomialWithoutHeadZeroes(S)
//            if (R.isEmpty()) { //000000000000
//                return listOf(BigInteger.ZERO)
//            }
            return if (cropLength == -1) {
                S
            } else {
                S.subList(0, minOf(cropLength, S.size)).toMutableList()
            }
        }

        fun mul(P: List<BigInteger>, Q: List<BigInteger>, cropLength: Int = -1): List<BigInteger> {
            if (P.isEmpty() || Q.isEmpty() || (P.size == 1 && P[0] == BigInteger.ZERO) || (Q.size == 1 && Q[0] == BigInteger.ZERO)) { //константный ноль
                return listOf(BigInteger.ZERO)
            }

            val S = ArrayList<BigInteger>()
            for (n in 0 until P.size + Q.size - 1) {
                var s = BigInteger.ZERO
                for (i in 0..n) {
                    s += (P.getI(i) * Q.getI(n - i))
                }
                S.add(s)
            }
//            val R = polynomialWithoutHeadZeroes(S)
//            if (R.isEmpty()) {
//                return listOf(BigInteger.ZERO)
//            }
            return if (cropLength == -1) { //S->R
                S
            } else {
                S.subList(0, minOf(cropLength, S.size)).toMutableList()
            }
        }

        fun div(P: List<BigInteger>, Q: List<BigInteger>, count : Int): List<BigInteger> {
            val S = ArrayList<BigInteger>()
//            val r = (reversedOnMod(Q[0], mod).first.mod(mod) + mod).mod(mod)
            //q[0] == 1 - всегда((((

            for (n in 0..count) {
                var sum = BigInteger.ZERO
                for (i in 0 until n) {
                    sum = (sum + (S.getI(i) * Q.getI(n - i)))
                }
                S.add((P.getI(n) - sum))
            }
            return S
        }

        fun List<BigInteger>.toPString() : String {
            if (this.isEmpty()) {
                return "0"
            }
            return this.joinToString(" ")
        }

        fun sqrt1plusPT(P: List<BigInteger>, mod: BigInteger, m: Int) : List<BigInteger> {
            var ans = listOf(BigInteger.ZERO)
            var PN = listOf(BigInteger.ONE)
            var factorialAndTwo = BigInteger.ONE
            var d: BigInteger
            var g = BigInteger.ONE
            for (i in 0..m) {
                ans = sum(ans, mul(PN, listOf((getReversedOnMod(factorialAndTwo, mod) * g).mod(mod)), m), m)

                PN = mul(PN, P, m)
                d = (BigInteger.ONE - i.toBigInteger() * BigInteger.TWO).mod(mod)
                g = (g * d).mod(mod)
                factorialAndTwo = (factorialAndTwo * (i.toBigInteger() + BigInteger.ONE) * BigInteger.TWO).mod(mod)
            }
            val ansWithZeroes = ArrayList(ans)
            for (i in ans.size..m) {
                ansWithZeroes.add(BigInteger.ZERO)
            }
            return ansWithZeroes.subList(0, m).toMutableList()
        }

        fun exp(P: List<BigInteger>, mod: BigInteger, m : Int) : List<BigInteger> {
            var ans = listOf(BigInteger.ZERO)
            var PN = listOf(BigInteger.ONE)
            var factorial = BigInteger.ONE
            for (i in 0..m) {
                ans = sum(ans, mul(PN, listOf(getReversedOnMod(factorial, mod)), m), m)
                PN = mul(PN, P, m)
                factorial = (factorial * (i.toBigInteger() + BigInteger.ONE)).mod(mod)
            }
            val ansWithZeroes = ArrayList(ans)
            for (i in ans.size..m) {
                ansWithZeroes.add(BigInteger.ZERO)
            }
            return ansWithZeroes.subList(0, m).toMutableList()
        }

        fun log(P: List<BigInteger>, mod: BigInteger, m : Int) : List<BigInteger> {
            var ans = listOf(BigInteger.ZERO)
            var PN = P
            var d = BigInteger.ONE
            for (i in 1..m) {
                ans = if (i % 2 == 1) {
                    sum(ans, mul(PN, listOf(getReversedOnMod(d, mod)), m), m)
                } else {
                    sub(ans, mul(PN, listOf(getReversedOnMod(d, mod)), m), m)
                }
                PN = mul(PN, P, m)
                d = (d + BigInteger.ONE).mod(mod)
            }
            val ansWithZeroes = ArrayList(ans)
            for (i in ans.size..m) {
                ansWithZeroes.add(BigInteger.ZERO)
            }
            return ansWithZeroes.subList(0, m).toMutableList()
        }

        fun shiftToRightAndAdd(A: List<BigInteger>, e : BigInteger = BigInteger.ZERO) : List<BigInteger> {
            val list = ArrayList<BigInteger>()
            list.add(e)
            list.addAll(A)
            return list
        }

        fun getPQOnRecompetitive(A : List<BigInteger>, C : List<BigInteger>) : Pair<List<BigInteger>, List<BigInteger>> {
            val Q = sub(listOf(BigInteger.ZERO), shiftToRightAndAdd(C, -BigInteger.ONE))
            val P = ArrayList<BigInteger>()
            val AA = ArrayList<BigInteger>()
            for (i in A.indices) {
                AA.add(BigInteger.ZERO)
            }
            AA.addAll(A)
            for (i in 0..Q.size - 2) {
                //p_i = a_i - a_i-1 * c_1 - ... a_i -j * c_j
                var sum = BigInteger.ZERO
                for (j in A.indices) {
                    sum += AA[A.size + i - j - 1] * C[j]
                }
                P.add(A[i] - sum)
            }
            return Pair(polynomialWithoutHeadZeroes(P), Q)
        }

        fun getValI(P : List<BigInteger>, cur : BigInteger) : BigInteger {
            var s = BigInteger.ZERO
            var c = BigInteger.ONE
            for (i in P.indices) {
                s = s + c * P[i]
                c = c * cur
            }
            return s
        }

        fun getPQon1KvaziPolynom(r : BigInteger, F : List<BigInteger>) : Pair<List<BigInteger>, List<BigInteger>> {
            var Q = listOf(BigInteger.ONE)
            val R = listOf(BigInteger.ONE, -(r))
            for (i in F.indices) {
                Q = mul(Q, R)
            }
            val C = ArrayList<BigInteger>()
            for (i in 1 until Q.size) {
                C.add(-Q[i])
            }
            val A = ArrayList<BigInteger>()
            var rn = BigInteger.ONE
            for (i in 0..Q.size - 2) {
                A.add(rn * getValI(F, i.toBigInteger()))
                rn = rn * r
            }
            return Pair(getPQOnRecompetitive(A, C).first, Q)
        }

        fun getDeterminant(matrix1 : ArrayList<ArrayList<BigInteger>>) : BigInteger {
            val matrix = matrix1.toMutableList()
            var ans = BigInteger.ONE
            var g = BigInteger.ONE
            var sign = 1
            for (i in matrix.indices) {
                if (matrix[i][i] == BigInteger.ZERO) { //переставляем строки
                    for (j in i until matrix.size) {
                        if (matrix[j][i] != BigInteger.ZERO) {
                            val I = matrix[i].toMutableList()
                            matrix[i] = matrix[j]
                            matrix[j] = ArrayList(I)
                            sign *= -1
                            break
                        }
                    }
                }

                val d = matrix[i][i]
                for (j in i+1 until matrix.size) { //вычитание
                    val x = BigInteger(matrix[j][i].toString())
                    g *= d
                    //d * x
                    matrix[j] = ArrayList(mul(listOf(d), matrix[j]))
                    matrix[j] = ArrayList(sub(matrix[j], mul(listOf(x), matrix[i])))
                }
                ans *= d
            }
            ans /= g
            return if (sign == 1) {
                ans
            } else {
                -ans
            }
        }

        fun getSolutionOfSystem(
                matrix : ArrayList<ArrayList<BigInteger>>,
                vector : List<BigInteger>
        ) : List<Pair<BigInteger, BigInteger>> {
            val dt = getDeterminant(matrix)
            val ans = ArrayList<Pair<BigInteger, BigInteger>>()
            for (i in vector.indices) {
                val vectorCopy = ArrayList<BigInteger>()
                for (j in vector.indices) {
                    vectorCopy.add(BigInteger(matrix[j][i].toString()))
                    matrix[j][i] = vector[j]
                }
//                println()
//                println(matrix)
                val dx = getDeterminant(matrix)
                for (j in vector.indices) {
                    matrix[j][i] = vectorCopy[j]
                }
                //println(dx)
                val gcd = dt.gcd(dx)
                ans.add(Pair(dx/gcd, dt/gcd))
            }
            return ans
        }


        fun getKvaziPolynomOnPr(r : BigInteger, P : List<BigInteger>) : List<Pair<BigInteger, BigInteger>> {
            val matrix = ArrayList<ArrayList<BigInteger>>()
            var rn = BigInteger.ONE
            for (i in P.indices) {
                val c = i.toBigInteger()
                var n = BigInteger.ONE
                val equation = ArrayList<BigInteger>()
                for (j in P.indices) {
                    if (i == 0 && j == 0) {
                        equation.add(BigInteger.ONE)
                    } else {
                        equation.add(n * rn)
                    }
                    n *= c
                }
                rn *= r
                matrix.add(equation)
            }
            var Q = listOf(BigInteger.ONE)
            val Q1 = listOf(BigInteger.ONE, -r)
            for (i in P.indices) {
                Q = mul(Q, Q1)
            }
            val vector = div(P, Q, P.size - 1)
//            println(matrix)
//            println(vector)
            return getSolutionOfSystem(matrix, vector)
        }

    }
}


fun main() {
    val (r, k) = readln().split(" ").map(String::toBigInteger)
    val P = readln().split(" ").map(String::toBigInteger)
    val R = getKvaziPolynomOnPr(r, P)
    for (i in R) {
        if (i.second < BigInteger.ZERO ) {
            print("${-i.first}/${-i.second} ")
        } else {
            print("${i.first}/${i.second} ")
        }
    }
}
