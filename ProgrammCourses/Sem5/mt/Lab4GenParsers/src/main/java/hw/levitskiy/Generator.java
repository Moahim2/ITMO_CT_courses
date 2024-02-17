package hw.levitskiy;

import hw.levitskiy.grammar.GrammarProcessor;
import hw.levitskiy.lexer.LexerGenerator;
import hw.levitskiy.parser.ParserGenerator;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;

import java.io.IOException;

public class Generator {

    public static void main(String[] args) throws IOException {
        generate(CharStreams.fromFileName(
                "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\" +
                        "Lab4GenParsers\\src\\main\\my_grammar\\hw\\levitskiy\\lab2grammar.g"
        ));

        generate(CharStreams.fromFileName(
                "C:\\Users\\Ivanl\\IdeaProjects\\DM5sem\\" +
                        "Lab4GenParsers\\src\\main\\my_grammar\\hw\\levitskiy\\calculator.g"
        ));
    }

    public static void generate(CharStream grammarInput) {
        GrammarProcessor grammarProcessor = new GrammarProcessor(grammarInput);
        LexerGenerator.generate(grammarProcessor);
        ParserGenerator.generate(grammarProcessor);
    }
}
