import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File

fun main() {
    val istream = BufferedReader(File("check.in").inputStream().reader())
    val (n, m) = istream.readLine().split(" ").map { it.toInt() }
    val arrayOfSets = Array<ArrayList<HashSet<Int>>>(n + 1) {
        ArrayList()
    }

    repeat(m) {
        val str = istream.readLine().split(" ")
        val size = str[0].toInt()
        val newSet = HashSet<Int>()
        for (j in 1..size) {
            newSet.add(str[j].toInt())
        }
        arrayOfSets[size].add(newSet)
    }

    istream.close()
    val out = BufferedWriter(File("check.out").outputStream().writer())

    if (arrayOfSets[0].size == 0) { //
        out.write("NO")
        out.close()
        return
    }

    var flag = false
    loop@ for (s in 1..n) {
        for (M in arrayOfSets[s]) {
            for (e in M) {
                val x = HashSet<Int>()
                x.addAll(M)
                x.remove(e)
                if (!arrayOfSets[s - 1].contains(x)) {
                    flag = true //проиграли!
                    break@loop
                }
            }
        }
    }

    if (flag) {
        out.write("NO")
        out.close()
        return
    }

    loop1@ for (s in 1..n) {
        for (A in arrayOfSets[s]) {
            for (B in arrayOfSets[s - 1]) {
                var bl2 = false
                for (e in A) {
                    if (!B.contains(e)) { //A > B
                        //println(B)
                        val x = HashSet<Int>()
                        x.addAll(B)
                        x.add(e)
                        if (arrayOfSets[s].contains(x)) {
                            //println("$A $B")
                            bl2 = true //выиграли
                            break
                        }
                    }
                }
                if (!bl2) {
                    flag = true //проиграли
                    break@loop1
                }
            }
        }
    }

    if (flag) {
        out.write("NO")
        out.close()
    } else {
        out.write("YES")
        out.close()
    }

}