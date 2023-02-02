import java.util.*

fun main() {
    val n = readln().toInt()
    val graph : ArrayList<HashSet<Int>> = ArrayList()
    for (i in 0..n) {
        graph.add(HashSet())
    }

    for (str in 1 until n) {
        val inp = readln().split(" ")
        val i = inp[0].toInt()
        val j = inp[1].toInt()
        graph[i].add(j)
        graph[j].add(i)
    }

    val lists = TreeSet<Int>()
    for (i in 1 until n) {
        if (graph[i].size == 1) {
            lists.add(i)
        }
    }


    val ans = ArrayList<Int>()
    var min = lists.first()
    for (num in 1..n-2) {
        val j = graph[min].first()
        ans.add(j)
//        for (v in graph[min]) {
//            graph[v].remove(min)
//            if (graph[v].size == 1) {
//                lists.add(v)
//            }
//        }
        graph[j].remove(min)
        if (graph[j].size == 1) {
            lists.add(j)
        }
        lists.remove(min)
        min = lists.first()
    }


    for (i in ans) {
        print("$i ")
    }
}
