
import java.util.*


fun main() {
    val  inputNM = readln().split(" ")
    val n = inputNM[0].toInt()
    val m = inputNM[1].toInt()

    val graph = ArrayList<HashSet<Int>>()
    val flags = ArrayList<Boolean>()
    val degs = ArrayList<Int>()
    val colors = ArrayList<Int>()
    for (i in 0..n) {
        graph.add(HashSet())
        flags.add(true)
        degs.add(0)
        colors.add(-1)
    }


    var k = 0
    for (i in 1..m) {
        val inputE = readln().split(" ")
        val l = inputE[0].toInt()
        val r = inputE[1].toInt()
        graph[l].add(r)
        graph[r].add(l)
        degs[l]++
        degs[r]++
        if (k < degs[l]) {
            k = degs[l]
        }
        if (k < degs[r]) {
            k = degs[r]
        }
    }

    //println(k)
    if (k % 2 == 0) {
        k++
    }

    var vWithSmallDeg = 0
    for (i in 1..n) {
        if (degs[i] < k) {
            vWithSmallDeg = i
            break
        }
    }

    val listBFS = ArrayList<Int>()

    listBFS.add(vWithSmallDeg)
    flags[vWithSmallDeg] = false


    val numbersInBFSPath = ArrayList<Int>()

    while (listBFS.isNotEmpty()) {
        val i = listBFS.removeAt(listBFS.size - 1)
        numbersInBFSPath.add(i)
        for (v in graph[i]) {
            if (flags[v]) {
                listBFS.add(v)
                flags[v] = false
            }
        }
    }

    //println(numbersInBFSPath)

    for (i_pair_ in 1..numbersInBFSPath.size) {
        val i_pair = numbersInBFSPath.size - i_pair_
        var minColour = 1
        val i = numbersInBFSPath[i_pair]
        val listCol = TreeSet<Int>()
        for (v in graph[i]) {
            if (colors[v] != -1) {
                listCol.add(colors[v])
            }
        }
        for (c in listCol) {
            if (minColour == c) {
                minColour++
            } else {
                break
            }
        }
        colors[i] = minColour
    }



    println(k)
    for (i in 1..n) {
        println(colors[i])
    }


}