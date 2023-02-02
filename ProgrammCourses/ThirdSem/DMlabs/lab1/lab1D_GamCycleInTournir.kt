//var n : Int = 0
//var graph : ArrayList<HashSet<Int>> = ArrayList()
//val flagsForStartDFS = ArrayList<Boolean>()
//var flagsForFindPath = HashSet<Int>()


//
//
//
//
//
//fun startDFS(v : Int, startV : Int) : Pair<ArrayList<Int>?, Boolean> {
//    flagsForStartDFS[v] = false
//
//    for (u in graph[v]) {
//        if (flagsForStartDFS[u]) {
//            val pair = startDFS(u, startV)
//            if (pair.second) {
//                pair.first?.add(v)
//                return Pair(pair.first, true)
//            }
//        } else if (u == startV) {
//            return Pair(arrayListOf(v), true)
//        }
//    }
//    return Pair(null, false)
//}
//
//fun findPath(v : Int, finishV : Int) : Pair<ArrayList<Int>?, Boolean> {
//    flagsForFindPath.add(v)
//
//    for (u in graph[v]) {
//        if (u == finishV) {
//            return Pair(arrayListOf(v), true)
//        }
//        if (!flagsForFindPath.contains(u)) {
//            val pair = findPath(u, finishV)
//
//            if (pair.second) {
//                pair.first?.add(v)
//                return Pair(pair.first, true)
//            }
//        }
//    }
//    return Pair(null, false)
//}
//
//
//fun main() {
//    n = readln().toInt()
//    for (i in 0..n) {
//        graph.add(HashSet())
//        flagsForStartDFS.add(true)
//    }
//
//    for (i in 1..n) {
//        val inputS = readln()
//        for (j in inputS.indices) {
//            if (inputS[j] == '1') {
//                graph[i].add(j + 1)
//            } else {
//                graph[j + 1].add(i)
//            }
//        }
//    }
//    if (n == 1) {
//        print(1)
//        return
//    }
//    //println(graph)
//   // val test = (1..n).random()
//
//    val startCycle : ArrayList<Int> = startDFS(1, 1).first!!
//    startCycle.reverse()
//    //println(startCycle)
//
//    var cycle = ArrayList<Int>()
//    for (j in startCycle) {
//        cycle.add(j.toString().toInt())
//    }
//    val nonCycle = HashSet<Int>()
//
//    for(i in 1..n) {
//        nonCycle.add(i)
//    }
//
//    for(i in cycle) {
//
//        nonCycle.remove(i)
//    }
//
//
//    while (cycle.size != n) {
//        //val newV = nonCycle.toArray()[(0 until nonCycle.size).random()] as Int
//        val newV = nonCycle.first()
//        var flag = true
//
//        for (i in 0 until cycle.size) {
//            if (i != cycle.size - 1) {
//                if (graph[cycle[i]].contains(newV) && graph[newV].contains(cycle[i + 1])) {
//                    flag = false
//                    val tmp = ArrayList<Int>()
//                    tmp.addAll(cycle)
//                    cycle = ArrayList()
//                    for (j in 0..i) {
//                        cycle.add(tmp[j])
//                    }
//                    cycle.add(newV)
//
//                    for (j in i+1 until tmp.size) {
//                        cycle.add(tmp[j])
//                    }
//                    break
//                }
//            } else {
//                if (graph[cycle[i]].contains(newV) && graph[newV].contains(cycle[0])) {
//                    flag = false
//                    cycle.add(newV)
//                    break
//                }
//            }
//        }
//
//        if (!flag) {
//            nonCycle.remove(newV)
//           // println("easy :   $cycle")
//            continue
//        }
//
//        flagsForFindPath = HashSet()
//
//        if (graph[cycle.first()].contains(newV)) { //cycle -> newV
//            val path = findPath(newV, cycle.first()).first!!
//            path.reverse()
//
//            path.add(cycle.first())
//
//            var lastP = 0
//            for (t in path.indices) {
//                if (!nonCycle.contains(path[t])) {
//                    lastP = t
//                    break
//                }
//            }
//
//            val x = cycle.indexOf(path[lastP]) - 1
//            val tmp = ArrayList<Int>()
//            tmp.addAll(cycle)
//            cycle = ArrayList()
//            for (j in 0..x) {
//                cycle.add(tmp[j])
//            }
//
//            for (ch in 0 until lastP) {
//                cycle.add(path[ch])
//                nonCycle.remove(path[ch])
//            }
//
//            for (j in x+1 until tmp.size) {
//                cycle.add(tmp[j])
//            }
//
//           // println("hard1 : $cycle")
//        } else { //cycle <- newV
//
//            val path = findPath(cycle.last(), newV).first!!
//            path.reverse()
//            var lastP = 0
//
//            for (t in path.size-1 downTo 0) {
//                if (!nonCycle.contains(path[t])) {
//                    lastP = t
//                    break
//                }
//            }
//
//            val x = cycle.indexOf(path[lastP])
//            val tmp = ArrayList<Int>()
//            tmp.addAll(cycle)
//            cycle = ArrayList()
//            for (j in 0..x) {
//                cycle.add(tmp[j])
//            }
//
//            for (ch in lastP + 1 until path.size) {
//
//                cycle.add(path[ch])
//                nonCycle.remove(path[ch])
//            }
//
//            cycle.add(newV)
//            nonCycle.remove(newV)
//
//            for (j in x+1 until tmp.size) {
//                cycle.add(tmp[j])
//            }
//
//            //println("hard2 : $cycle")
//        }
//    }
//
//    for (i in cycle) {
//        print("$i ")
//    }
//    println()
//    for (i in 0 until cycle.size - 1) {
//        if (i != cycle.size - 1) {
//            if (!graph[cycle[i]].contains(cycle[i + 1])) {
//                print("FAIL : $i ${i + 1} : not e : $test")
//                break
//            }
//        } else {
//            if (!graph[cycle[i]].contains(0)) {
//                print("FAIL : $i ${0} : not e : $test")
//            }
//        }
//    }
//}