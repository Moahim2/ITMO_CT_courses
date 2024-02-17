package levitskiy.hw;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;


public class FileTranslator {

    public static void translateAndSaveToFile(String name, String text, boolean log) {
        translateAndSaveToFile(name, CharStreams.fromString(text), log);
    }

    public static void translateAndSaveToFile(String name, CharStream inputStream, boolean log) {
        PrefixLanguageLexer lexer = new PrefixLanguageLexer(inputStream);
        PrefixLanguageParser parser = new PrefixLanguageParser(new CommonTokenStream(lexer));

        VisitorTranslator visitorTranslator = new VisitorTranslator();
        String result = visitorTranslator.visit(parser.program());

        if (log) {
            System.out.println(result);
        }

        if (name == null) {
            return;
        }

        try {
            Files.writeString(Path.of(name), result);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}

