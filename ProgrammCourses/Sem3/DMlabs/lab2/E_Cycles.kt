import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File

fun main() {

    val istream = BufferedReader(File("cycles.in").inputStream().reader())
    val (n, m) = istream.readLine().split(" ").map { it.toInt() }
    val arrayOfSets = Array<ArrayList<HashSet<Int>>>(n + 1) {
        ArrayList()
    }

    val weights = Array(n) {
        Pair(0, 0)
    }

    val inp = istream.readLine().split(" ")
    for (i in 0 until n) {
        weights[i] = Pair(inp[i].toInt(), i + 1)
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

    val comporator = kotlin.Comparator<Pair<Int, Int>> { p1: Pair<Int, Int>, p2: Pair<Int, Int> ->
        return@Comparator -p1.first.compareTo(p2.first)
    }
    weights.sortWith(comporator)


    val ansSet = HashSet<Int>()
    var ansWeight = 0
    for (e in weights) {
        ansSet.add(e.second)
        var flag = false
        loop@ for (size in 0..ansSet.size) {
           for (set in arrayOfSets[size]) {
               if (ansSet.containsAll(set)) {
                   flag = true
                   break@loop
               }
           }
        }

        if (flag) { //есть такой цикл
            //println(ansSet)
            ansSet.remove(e.second)
        } else {
            ansWeight += e.first
            //println(ansWeight)
        }
    }
    val out = BufferedWriter(File("cycles.out").outputStream().writer())
    //println(ansWeight)
    out.write(ansWeight.toString())
    out.close()
}
