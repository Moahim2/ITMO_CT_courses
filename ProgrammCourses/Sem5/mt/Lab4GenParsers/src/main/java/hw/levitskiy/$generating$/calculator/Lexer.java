package hw.levitskiy.$generating$.calculator;

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

    private final Pattern skip = Pattern.compile("[ \n\r\t\f]");

    private Reader input;
    private int curPos = 0;
    private char curChar = ' ';
    private Token curToken;

    private static final char END_CHAR = (char) -1;

    private final Map<String, String> tokenBijectionImpl = Map.of(
        "DIV", "\\/",
        "RB", "\\)",
        "ADD", "\\+",
        "SUB", "\\-",
        "MUL", "\\*",
        "FTR", "\\!",
        "LB", "\\(",
        "NUM", "^\\d+$");

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

}