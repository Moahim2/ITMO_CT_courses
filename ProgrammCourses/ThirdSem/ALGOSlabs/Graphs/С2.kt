import java.util.*
import kotlin.collections.ArrayList
import kotlin.collections.HashMap
import kotlin.collections.HashSet

fun main() {
    val (n, m) = readln().split(" ").map { it.toInt() }
    val names =  TreeMap<String, Int>()
    val reverseNames = TreeMap<Int, String>()
    val graph = TreeMap<Int, TreeSet<Int>>()
    val invGraph = TreeMap<Int, TreeSet<Int>>()
    val vis = TreeMap<Int, Boolean>()
    val components = TreeMap<Int, Int>()
    for (i in 1..n) {
        val str = readln()
        names[str] = i
        reverseNames[i] = str
        graph[i] = TreeSet()
        graph[-i] = TreeSet()
        invGraph[i] = TreeSet()
        invGraph[-i] = TreeSet()
        vis[i] = false
        vis[-i] = false
        components[i] = -1
        components[-i] = -1
    }
//    for (i in 1..n) {
//        graph[-i]?.add(i)
//        invGraph[i]?.add(-i)
//    }

    for (i in 0 until m) {
        val line = readln().split(" => ")
        val u = if (line[0][0] == '-') {
            -names[line[0].substring(1)]!!
        } else {
            names[line[0].substring(1)]!!
        }

        val v = if (line[1][0] == '-') {
            -names[line[1].substring(1)]!!
        } else {
            names[line[1].substring(1)]!!
        }

        graph[u]?.add(v)
        invGraph[v]?.add(u)

        graph[-v]?.add(-u)
        invGraph[-u]?.add(-v)
    }


    val p = ArrayList<Int>()
    fun dfs1(u : Int) {
        vis[u] = true
        for (v in graph[u]!!) {
            if (!vis[v]!!) {
                dfs1(v)
            }
        }
        p.add(u)
    }
    for (i in vis.keys) {
        if (!vis[i]!!) {
            dfs1(i)
        }
    }

    var curComp = 0
    //val graphVonComp = ArrayList<HashSet<Int>>()
    val graphComponents = ArrayList<HashSet<Int>>()
    fun dfs2(u : Int) {
        components[u] = curComp
        //graphVonComp[curComp].add(u)
        for (v in invGraph[u]!!) {
            if (components[v] == -1) {
                dfs2(v)
            } else if (components[v] != curComp) {
                graphComponents[components[v]!!].add(curComp)
            }
        }
    }
    p.reverse()
    for (v in p) {
        if (components[v] == -1) {
            graphComponents.add(HashSet())
            dfs2(v)
            curComp++
        }
    }
    //println(graphComponents)
    val ord = ArrayList<Int>()
    val flags = Array(curComp) {
        false
    }
    fun dfs3(u : Int) {
        flags[u] = true
        for (v in graphComponents[u]) {
            if (!flags[v]) {
                dfs3(v)
            }
        }
        ord.add(u)
    }
    for (i in flags.indices) {
        if (!flags[i]) {
            dfs3(i)
        }
    }
    ord.reverse()

    val map = HashMap<Int, Int>()
    for (i in 0 until curComp) {
        map[ord[i]] = i
    }
    //println(ord)
    var flag = false
//    loop@ for (c in graphVonComp) {
//        for (v in c) {
//            if (c.contains(-v)) {
//                flag = true
//                break@loop
//            }
//        }
//    }
//    if (flag) {
//        println(-1)
//        return
//    }


    val ans = ArrayList<String>()
    for (i in 1..n) {
        if (map[components[i]!!]!! > map[components[-i]!!]!!) {
            ans.add(reverseNames[i]!!)
        }
//        else if (components[i] == components[-i]) {
//            println(-1)
//            return
//        }
    }
    //println(components)
    if (ans.size != 0) {

        println(ans.size)
        for (i in ans) {
            println(i)
        }
    } else {
        println(-1)
    }
}