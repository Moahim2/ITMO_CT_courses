package hw.levitskiy.calculator.visualisation

import hw.levitskiy.calculator.visualisation.CreatorVisualisation.Companion.create
import hw.levitskiy.`$generating$`.calculator.Parser
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
    create("1 + 1", "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\Lab4GenParsers\\src\\test\\kotlin\\hw\\levitskiy\\calculator\\visualisation\\pictures\\p1.gv")

    create("(4 * 1 + (2) * 3) + ((1 - 7) / 5 + (1 + 2 + (3 * 3 + 7) * 11 - 1 - (1 - (2 * 3 - (1 + (11 - 10000))))))",
        "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\Lab4GenParsers\\src\\test\\kotlin\\hw\\levitskiy\\calculator\\visualisation\\pictures\\p2.gv")
}
