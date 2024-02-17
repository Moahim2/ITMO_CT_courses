package hw.levitskiy.lab2.visualisation

import hw.levitskiy.lab2.visualisation.CreatorVisualisation.Companion.create
import hw.levitskiy.`$generating$`.lab2.Parser
import java.nio.file.Files
import java.nio.file.Path


class CreatorVisualisation {

    companion object {
        fun create(inputLine : String, outFileName : String) {
            val outFile = Path.of(outFileName)

            Files.newBufferedWriter(outFile).use {
                val p = Parser()
                val tree = p.parse(inputLine.reader()).second

                it.write("digraph Tree {")
                it.newLine()

                tree.toDot(it)

                it.write("label=\"$inputLine\";")
                it.newLine()
                it.write("}")
            }

            ProcessBuilder("cmd.exe")
                .command(
                    "cmd",
                    "/c",
                    "dot -Tpng ${outFile.fileName} -o${outFile.fileName}.png")
                .directory(outFile.toFile().parentFile)
                .start()
        }
    }
}

fun main() {

    create("var x: Array<Int>;", "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\Lab4GenParsers\\src\\test\\kotlin\\hw\\levitskiy\\lab2\\visualisation\\pictures\\p1.gv")
    create("var      x: Array<Map<Int>>;", "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\Lab4GenParsers\\src\\test\\kotlin\\hw\\levitskiy\\lab2\\visualisation\\pictures\\p2.gv")
    create("var x: Array<Array<Int>>;", "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\Lab4GenParsers\\src\\test\\kotlin\\hw\\levitskiy\\lab2\\visualisation\\pictures\\p3.gv")
    create("var array: Array<Map<Int, String>>;", "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\Lab4GenParsers\\src\\test\\kotlin\\hw\\levitskiy\\lab2\\visualisation\\pictures\\p4.gv")
    create("var xyz: Array<RandomClass<Int,Int,Int,  String, T, T, T>>;", "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\Lab4GenParsers\\src\\test\\kotlin\\hw\\levitskiy\\lab2\\visualisation\\pictures\\p5.gv")
    create("var name   : Array<RandomClass<Int, Map<Map<T,T,Map<B,B,B,B>,T>,Int>>>;", "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\Lab4GenParsers\\src\\test\\kotlin\\hw\\levitskiy\\lab2\\visualisation\\pictures\\p6.gv")
    create("var Array: Array<Array<Int>>;", "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\Lab4GenParsers\\src\\test\\kotlin\\hw\\levitskiy\\lab2\\visualisation\\pictures\\p7.gv")

//    create("var n : Map<E, E>;", "src/pictures/p8.gv")
}
