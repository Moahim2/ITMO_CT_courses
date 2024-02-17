package hw.levitskiy.lexer;

import hw.levitskiy.exception.GenException;
import hw.levitskiy.exception.LexerGenException;
import hw.levitskiy.grammar.GrammarProcessor;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.Map;

import static hw.levitskiy.util.Util.TAB;

public class LexerGenerator {

    public static void generate(GrammarProcessor grammarProcessor) throws GenException {
        try {
            String subPackage = grammarProcessor.getPackage();
            generateTerminals(grammarProcessor, subPackage);
            generateToken(subPackage);
            generateLexer(grammarProcessor, subPackage);
        } catch (IOException e) {
            throw new LexerGenException("Cannot write or create file in the specified package.", e);
        }
    }

    private static void generateTerminals(GrammarProcessor grammarProcessor, String subPackage) throws IOException {
        String dir = "src/main/java/hw/levitskiy/$generating$/" + subPackage;
        Files.createDirectories(Path.of(dir));

        BufferedWriter writer = new BufferedWriter(new FileWriter(dir + "/Terminal.java"));
        writer.write("package hw.levitskiy.$generating$." + subPackage + ";\n");
        writer.write("\npublic enum Terminal {\n");
        writer.write("%s$ ()".formatted(TAB));
        for (var p : grammarProcessor.getTokens().entrySet()) {
            writer.write(",\n%s%s ()".formatted(TAB, p.getKey()));
        }

        writer.write("""
                ;

                    Terminal() {

                    }
                }
                """);
        writer.close();
    }

    private static void generateToken(String subPackage) throws IOException {
        String dir = "src/main/java/hw/levitskiy/$generating$/" + subPackage;
        Files.createDirectories(Path.of(dir));

        BufferedWriter writer = new BufferedWriter(new FileWriter(dir + "/Token.java"));
        writer.write("package hw.levitskiy.$generating$." + subPackage + ";\n\n");
        writer.write("""
                public class Token {
                    private final String textVal;
                    private final Terminal terminal;

                    public Token(String textVal, Terminal terminal) {
                        this.textVal = textVal;
                        this.terminal = terminal;
                    }


                    public String getTextVal() {
                        return textVal;
                    }

                    public Terminal getTerminal() {
                        return terminal;
                    }
                }""");
        writer.write("\n");
        writer.close();
    }

    private static void generateLexer(GrammarProcessor grammarProcessor, String subPackage) throws IOException {
        String dir = "src/main/java/hw/levitskiy/$generating$/" + subPackage;

        BufferedWriter writer = new BufferedWriter(new FileWriter(dir + "/Lexer.java"));
        writer.write("package hw.levitskiy.$generating$." + subPackage + ";\n");
        writer.write("""

                import hw.levitskiy.exception.LexerGenException;

                import java.io.IOException;
                import java.io.Reader;
                import java.text.ParseException;
                import java.util.Map;
                import java.util.regex.Pattern;
                import java.util.regex.PatternSyntaxException;
                import java.util.stream.Collectors;

                public class Lexer {
                    private final Map<String, Pattern> tokenBijection;

                    private final Pattern skip = %s;

                    private Reader input;
                    private int curPos = 0;
                    private char curChar = ' ';
                    private Token curToken;

                    private static final char END_CHAR = (char) -1;

                """.formatted("Pattern.compile(%s)".formatted(grammarProcessor.getSkip())));
        writer.write("%sprivate final Map<String, String> tokenBijectionImpl = Map.of(\n".formatted(TAB));

        for (Iterator<Map.Entry<String, String>> iterator = grammarProcessor.getTokens().entrySet().iterator(); iterator.hasNext(); ) {
            var p = iterator.next();
            writer.write("%s%s\"%s\", \"%s\"".formatted(TAB, TAB, p.getKey(), p.getValue()));
            if (iterator.hasNext()) {
                writer.write(",\n");
            }
        }

        writer.write(");\n\n");
        writer.write(TAB + """
                public Lexer() {
                        try {
                            this.tokenBijection = tokenBijectionImpl
                                    .entrySet()
                                    .stream()
                                    .collect(Collectors.toMap(
                                            Map.Entry::getKey,
                                            p -> Pattern.compile(p.getValue())));
                    
                        } catch (PatternSyntaxException e) {
                            throw new LexerGenException(
                                    "Bad grammar syntax for lexer tokens, it must be correct java regular expression",
                                    e
                            );
                        }
                    }
                    
                    public void setInput(Reader input) throws ParseException {
                        this.input = input;
                        nextChar();
                        curPos = 0;
                    }
                    
                    public Token nextToken() throws ParseException {
                        while(skip.matcher(Character.toString(curChar)).matches()) {
                            nextChar();
                        }
                    
                        //abracadabra  Array
                        String curTokenStr = Character.toString(curChar);
                        while (curChar != END_CHAR && !skip.matcher(Character.toString(curChar)).matches()) {
                            Token token = checkTerminalAndGet(curTokenStr);
                    
                            nextChar();
                            curTokenStr += curChar;
                            if (token == null) {
                                continue;
                            }
                    
                            Token nextToken = checkTerminalAndGet(curTokenStr);
                            if (nextToken != null) {
                                continue;
                            }
                            curToken = token;
                            return token;
                        }
                        if (curTokenStr.length() == 1 && curChar == END_CHAR) {
                            curToken = new Token("$", Terminal.$);
                            return new Token("$", Terminal.$);
                        }
                        throw new ParseException("Illegal token " + curTokenStr, curPos);
                    }
                    
                    public Token curToken() {
                        return curToken;
                    }
                    
                    public int curPos() {
                        return curPos;
                    }
                    
                    private Token checkTerminalAndGet(String token) {
                        for (var p : tokenBijection.entrySet()) {
                            if (p.getValue().matcher(token).matches()) {
                                return new Token(token, Terminal.valueOf(p.getKey()));
                            }
                        }
                        return null;
                    }
                    
                    
                    private void nextChar() throws ParseException {
                        curPos++;
                        try {
                            curChar = (char) input.read();
                        } catch (IOException e) {
                            throw new ParseException(e.getMessage(), curPos);
                        }
                    }

                }""");

        writer.close();
    }

}
