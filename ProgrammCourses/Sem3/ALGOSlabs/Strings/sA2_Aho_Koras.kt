
class KaraseV1(val s : ArrayList<String>, val t : String) {
    val paths = ArrayList<HashMap<Char, Int>>()
    private val parent = ArrayList<Pair<Int, Char>>()
    private lateinit var suflink : IntArray
    private val alphabet = HashSet<Char>()
    private val terminals = HashMap<Int, Int>()
    private lateinit var compressedSuflink : IntArray
    var ansLink : IntArray
    var ansNotLink : IntArray
    private val BFS = ArrayList<Int>()


    init {
        createPaths()
        for (c in alphabet) {
            if (!paths[0].containsKey(c)) {
                paths[0][c] = 0
            }
        }
        findBFS()
        for (v in BFS) {
            if (parent[v].first != 0) {
                suflink[v] = paths[suflink[parent[v].first]][parent[v].second]!!
            } else {
                suflink[v] = 0
            }
            if (v != 0) {
                if (terminals.containsKey(suflink[v])) {
                    compressedSuflink[v] = suflink[v]
                } else if (terminals.containsKey(compressedSuflink[suflink[v]])) {
                    compressedSuflink[v] = compressedSuflink[suflink[v]]
                }
            }
            // println(paths)
            //println(suflink)
            for (c in alphabet) {
                if (!paths[v].containsKey(c)) {
                    paths[v][c] = paths[suflink[v]][c]!!
                }
            }
        }
        ansLink = IntArray(terminals.size)
        ansNotLink = IntArray(terminals.size)

//        for (u in compressedSuflink.indices) {
//            println("comp  $u : ${compressedSuflink[u]}")
//            println("sufl  $u : ${suflink[u]}")
//        }
       // println(terminals)
    }

    private fun createPaths() {
        paths.add(HashMap())
        var cur = 0
        for (c in t) {
            alphabet.add(c)
        }
        parent.add(Pair(0, 0.toChar())) ///мб бажит
        for (i in s.indices) {
            for (j in s[i]) {
                alphabet.add(j)
                if (!paths[cur].containsKey(j)) {
                    paths.add(HashMap())
                    paths[cur][j] = paths.size - 1
                    parent.add(Pair(cur, j))
                }
                cur = paths[cur][j]!!
            }
            terminals[cur] = i
            cur = 0
        }
        suflink = IntArray(paths.size)
        compressedSuflink = IntArray(paths.size) {
            -1
        }
        suflink[0] = 0
    }

    private fun findBFS() {
        val Q = ArrayDeque<Int>()
        val vis = BooleanArray(paths.size)
        Q.add(0)
        while (Q.isNotEmpty()) {
            val u = Q.removeFirst()
            BFS.add(u)
            vis[u] = true
            for (v in paths[u].values) {
                if (!vis[v]) {
                    Q.add(v)
                }
            }
        }
    }

    fun getHaveSubs(u : Int) {
        if (terminals.containsKey(u)) {
            ansNotLink[terminals[u]!!]++
        }
        if (compressedSuflink[u] != -1) {
            ansLink[terminals[compressedSuflink[u]]!!]++
        }
//        if (cur == 0) {
//            return -1
//        }
//        return terminals[cur]!!
    }


    fun finishSum() {
        BFS.reverse()
        for (v in BFS) {
            if (terminals.containsKey(v)) {
                ansNotLink[terminals[v]!!] += ansLink[terminals[v]!!]
                if (compressedSuflink[v] != -1) {
                    ansLink[terminals[compressedSuflink[v]]!!] += ansLink[terminals[v]!!]
                }
            }

        }
    }

}



fun main() {
    val n = readln().toInt()
    val s = ArrayList<String>()
    val realStr = HashMap<String, ArrayList<Int>>()
    repeat(n) {
        val str = readln()
        if (realStr.containsKey(str)) {
            realStr[str]?.add(it)
        } else {
            s.add(str)
            realStr[str] = arrayListOf(it)
        }
    }

    val t = readln()
    val myKarase = KaraseV1(s, t)
    var cur = 0


    for (i in t.indices) {
        //println(cur)
        cur = myKarase.paths[cur][t[i]]!!
        //println(cur)
        myKarase.getHaveSubs(cur)
    }
    val ans = Array(n) {
        0
    }
    //myKarase.ansLink.map { println(it) }
    myKarase.finishSum()
    for (i in s.indices) {
        for (j in realStr[s[i]]!!) {
            ans[j] = myKarase.ansNotLink[i]
        }
    }
    ans.map { println(it) }

}

