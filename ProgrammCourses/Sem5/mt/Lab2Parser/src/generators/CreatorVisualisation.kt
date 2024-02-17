package generators

import generators.CreatorVisualisation.Companion.create
import parser.Parser
import java.nio.file.Files
import java.nio.file.Path


class CreatorVisualisation {

    companion object {
        fun create(inputLine : String, outFileName : String) {
            val outFile = Path.of(outFileName)

            Files.newBufferedWriter(outFile).use {
                val p = Parser()
                val tree = p.parse(inputLine.reader())

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
    create("var x: Array<Int>;", "src/pictures/p1.gv")
    create("var      x: Array<Class<Int>>;", "src/pictures/p2.gv")
    create("var x: Array<Array<Int>>;", "src/pictures/p3.gv")
    create("var array: Array<Class<Int, String>>;", "src/pictures/p4.gv")
    create("var xyz: Array<RandomClass<Int,Int,Int,  String, T, T, T>>;", "src/pictures/p5.gv")
    create("var name   : Array<RandomClass<Int, MapT<MapT<T,T,MapT<B,B,B,B>,T>,Int>>>;", "src/pictures/p6.gv")
    create("var Array: Array<Array<Int>>;", "src/pictures/p7.gv")

    // modification with Maps (aka Array)
    create("var n : Map<E, E>;", "src/pictures/p8.gv")
}
