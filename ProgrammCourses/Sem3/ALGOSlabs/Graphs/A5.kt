fun main() {
    val n = readln().toInt()
    readln()
    val kArray = readln().split(" ").map { it.toInt() - 1}
    val graph = Array<HashSet<Int>>(n) {
        HashSet()
    }
    val m = readln().toInt()
    repeat(m) {
        val (i, j) = readln().split(" ").map { it.toInt() - 1 }
        graph[i].add(j)
        graph[j].add(i)
    }
    val d = Array(n) {
        -1
    }
    val nearest = Array(n) {
        -1
    }
    val queue = ArrayDeque<Int>()
    for (i in kArray) {
        d[i] = 0
        nearest[i] = i
        queue.add(i)
    }
    while (queue.isNotEmpty()) {
        val v = queue.removeFirst()
        for (u in graph[v]) {
            if (d[u] == -1) {
                d[u] = d[v] + 1
                nearest[u] = nearest[v]
                queue.add(u)
            } else if (d[u] == d[v] + 1 && nearest[v] < nearest[u]) {
                nearest[u] = nearest[v]
            }
        }
    }

    for (u in d) {
        print("$u ")
    }
    println()
    for (u in nearest) {
        print("${u + 1} ")
    }
}