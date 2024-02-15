
import java.util.*

var tableFullPolynomials = ArrayList<ArrayList<Int>>()

fun fillTable(max : Int, cur : Int = 0) {
    if (cur != 0) {
        val nextPolynomial = ArrayList<Int>()
        nextPolynomial.add(1)
        val k = cur
        for (i in 1..cur) {
            var last = 0
            if (tableFullPolynomials[cur - 1].size > i) {
                last = tableFullPolynomials[cur - 1][i]
            }
            nextPolynomial.add(last - k * tableFullPolynomials[cur - 1][i - 1])
        }
        tableFullPolynomials.add(nextPolynomial)
    } else {
        tableFullPolynomials.add(arrayListOf(1))
    }


    if (cur == max) {
        return
    } else {
        fillTable(max, cur + 1)
    }
}


fun getPolinomial(graph : ArrayList<TreeSet<Int>>) : ArrayList<Int> {

    var u = -1
    var v = -1
    for (i in 1 until graph.size) {
        if (graph[i].size != graph.size - 2) {
            u = i
            for (j in 1 until graph.size) {
                if (j != i && !graph[i].contains(j)) {
                    v = j
                    break
                }
            }
            break
        }
    }
    if (v == -1) { //полный граф //graph.size - 1 = k = deg// t * (t - 1) * (t - 2) *...* (t - (k - 1))
        return ArrayList(tableFullPolynomials[graph.size - 2].toList())
    }
   // println("$u, $v, g : $graph")
    val graph1 = ArrayList(graph.map { TreeSet(it.toSet()) })
    graph1[v].add(u)
    graph1[u].add(v)
   // println("$u, $v, g1 : $graph1")
    val polynomial1 = ArrayList(getPolinomial(graph1).toList())
  //  println("$u, $v, g : $graph")
    val graph2 = graph
    for (ver in graph2[v]) {
        graph2[u].add(ver)
    }

    for (i in 1 until graph2.size) {
        if (i != v) {
            if (graph2[i].contains(v)) {
                graph2[i].add(u)
                graph2[i].remove(v)
            }
            val change = ArrayList<Int>()
            for (ver in graph2[i]) {
                if (ver > v) {
                    change.add(ver)
                }
            }
            for (ver in change) {
                graph2[i].remove(ver)
                graph2[i].add(ver - 1)
            }
        }
    }
    graph2.removeAt(v)
   // println("$u, $v, g2 : $graph2")
    val polynomial2 = getPolinomial(graph2)
 //   polynomial2
    for (i in 0 until polynomial2.size) {
        polynomial1[i + 1] = polynomial1[i + 1] + polynomial2[i]
    }
    return polynomial1
}






fun main() {
    val inputNM = readln().split(" ")
    val n = inputNM[0].toInt()
    val m = inputNM[1].toInt()


    val startGraph = ArrayList<TreeSet<Int>>()
    for (i in 0..n) {
        startGraph.add(TreeSet())
    }

    for (s in 1..m) {
        val inp = readln().split(" ")
        val i = inp[0].toInt()
        val j = inp[1].toInt()
        startGraph[i].add(j)
        startGraph[j].add(i)
    }

    fillTable(n - 1)
   // println(tableFullPolynomials)
    //println(tableFullPolynomials)
    val ans = getPolinomial(startGraph)
    ans.add(0)
    println(ans.size - 1)
    for (i in ans) {
        print("$i ")
    }
}

