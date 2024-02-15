//var n : Int = 0
//var graph : Array<Array<Boolean>>? = null
//
//
//
//
//
//
//
//fun main() {
//    n = readln().toInt()
//    graph = Array(n + 1) {
//        Array(n + 1) { false }
//    }
//
//
//    for (i in 1..n) {
//        val inputS = readln()
//        for (j in inputS.indices) {
//            if (inputS[j] == '1') {
//                graph!![i][j + 1] = true
//                graph!![j + 1][i] = true
//            }
//        }
//    }
//    //println("AAAAAA")
//
//    val arrayDeq = ArrayDeque<Int>()
//    for (i in n downTo 1) {
//        arrayDeq.addLast(i)
//    }
//
//    var counter = 0
//
//    while (true) {
//        //println(AAAAAAAAAAAAAA)
//
//        if (counter == n + 1) {
//            break
//        }
//
//        val l = arrayDeq[0]
//        val r = arrayDeq[1]
//
//        if (graph!![l][r]) {
//            arrayDeq.removeFirst()
//            arrayDeq.addLast(l)
//            counter++
//            continue
//        }
//
//        /////////////////////////
//        counter = 0
//        var i_v = -1
//        for (i in 0 until n) {
//            if (graph!![arrayDeq[i]][l] && graph!![arrayDeq[i + 1]][r]) {
//                i_v = i
//                break
//            }
//        }
//
////        val copy = ArrayDeque<Int>()
////        copy.addAll(arrayDeq)
//
//        for (i in 0 until ((i_v + 1) / 2)) {
//            val tmp = arrayDeq[i_v - i]
//            arrayDeq[i_v - i] = arrayDeq[i + 1]
//            arrayDeq[i + 1] = tmp
//        }
//
////        arrayDeq
////        arrayDeq
//        arrayDeq.removeFirst()
//        arrayDeq.addLast(l)
//    }
//
//   for (v in arrayDeq) {
//       print("$v ")
//   }
//}