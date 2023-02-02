fun main() {
    val line = readln()


    val p = Array(line.length) {
        0
    }

    for (i in 1 until line.length) {
        var s = p[i - 1]
        while (s > 0 && line[i] != line[s]) {
            s = p[s - 1]
        }
        if (line[s] == line[i]) {
            s++
        }
        p[i] = s
    }

    val period = line.length - p[line.length - 1]
    //println(period)

    println(if (period <= line.length / 2) {
        period
    } else {
        line.length
    })

}
