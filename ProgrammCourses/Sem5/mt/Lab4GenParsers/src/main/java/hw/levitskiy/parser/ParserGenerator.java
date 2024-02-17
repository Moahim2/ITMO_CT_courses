package hw.levitskiy.parser;

import hw.levitskiy.exception.GenException;
import hw.levitskiy.exception.ParserGenException;
import hw.levitskiy.grammar.GrammarProcessor;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static hw.levitskiy.util.Util.*;

public class ParserGenerator {
    private final String startTerminal;

    private final Map<String, Set<RuleRightPart>> rules;
    private final Set<String> terminals;
    private final Map<String, Map.Entry<String, String>> signatures;

    private final Map<String, Set<String>> FIRST = new HashMap<>();
    private final Map<String, Set<String>> FOLLOW = new HashMap<>();

    private final BufferedWriter writer;

    public static void generate(GrammarProcessor grammarProcessor) throws GenException {
        try {
            ParserGenerator parserGenerator = new ParserGenerator(grammarProcessor);
            parserGenerator.generateImpl();
        } catch (IOException e) {
            throw new ParserGenException("Cannot write or create file in the specified package.", e);
        }
    }

    private ParserGenerator(GrammarProcessor grammarProcessor) throws IOException {
        writer = initWriterAndWritePackageName(grammarProcessor);

        startTerminal = grammarProcessor.getStartTerminal();
        rules = grammarProcessor.getRules();

        terminals = new HashSet<>(grammarProcessor.getTokens().keySet());
        terminals.add(EPS);
        terminals.add(END);

        signatures = grammarProcessor.getSignature();

        for (var s : rules.keySet()) {
            if (!signatures.containsKey(s)) {
                signatures.put(s, Map.entry(EMPTY_RETURN, EMPTY_ARG));
            }
        }

        for (var s : rules.keySet()) {
            FIRST.put(s, new HashSet<>());
        }

        for (var s : rules.keySet()) {
            FOLLOW.put(s, new HashSet<>());
        }
        FOLLOW.put(startTerminal, new HashSet<>(Set.of(END)));
    }

    @NotNull
    private BufferedWriter initWriterAndWritePackageName(GrammarProcessor grammarProcessor) throws IOException {
        final BufferedWriter writer;
        String subPackage = grammarProcessor.getPackage();
        String dir = "src/main/java/hw/levitskiy/$generating$/" + subPackage;
        Files.createDirectories(Path.of(dir));
        writer = new BufferedWriter(new FileWriter(dir + "/Parser.kt"));
        writer.write("package hw.levitskiy.`$generating$`." + subPackage + "\n\n");
        return writer;
    }

    private void generateImpl() throws IOException {
        calculateFirst();
        calculateFollow();
        System.out.println("FIRST: " + FIRST);
        System.out.println("FOLLOW: " + FOLLOW);
        if (!isLL1()) {
            throw new ParserGenException("Input grammar is not LL1!");
        } else {
            System.out.println("Is correct LL1 grammar!");
        }

        generateStartAndImports();

        for (var A : rules.keySet()) {
            generateNonTerminalFunction(A);
            writer.write("\n");
        }

        generateEnd();
    }

    private boolean isTerminal(String a) {
        return terminals.contains(a);
    }


//Calculate First-Follow
/////////////////////////////////////////////////////
    private Set<String> first(List<String> alpha) {
        if (alpha.size() == 1) {
            String a = alpha.get(0);
            if (a.equals(EPS)) {
                return new HashSet<>(Set.of(EPS));
            } else if (isTerminal(a)) {
                return new HashSet<>(Set.of(a));
            } else {
                return new HashSet<>(FIRST.get(a));
            }
        }

        String a = alpha.get(0);
        List<String> b = new ArrayList<>(alpha.stream().skip(1).toList());
        if (isTerminal(a)) {
            return new HashSet<>(Set.of(a));
        }

        Set<String> awe = new HashSet<>(FIRST.get(a));
        awe.remove(EPS);
        if (FIRST.get(a).contains(EPS)) {
            awe.addAll(first(b));
        }
        return awe;
    }

    private List<List<String>> getAllTrans(Map.Entry<String, Set<RuleRightPart>> rule) {
        return rule.getValue().stream().map(RuleRightPart::trans).toList();
    }

    private void calculateFirst() {
        while (true) {
            boolean flag = false;
            for (var rule : rules.entrySet()) {
                String A = rule.getKey();
                for (List<String> alpha : getAllTrans(rule)) {
                    if (FIRST.get(A).addAll(first(alpha))) {
                        flag = true;
                    }
                }
            }
            if (!flag) {
                break;
            }
        }
    }

    private void calculateFollow() {
        while (true) {
            boolean flag = false;
            for (var rule : rules.entrySet()) {
                String A = rule.getKey();
                for (List<String> alpha : getAllTrans(rule)) {
                    for (int i = 0; i < alpha.size(); i++) {
                        String B = alpha.get(i);
                        if (isTerminal(B)) {
                            continue;
                        }

                        List<String> gamma = new ArrayList<>(alpha.stream().skip(i + 1).toList());
                        if (gamma.isEmpty()) {
                            gamma.add(EPS);
                        }

                        Set<String> R = new HashSet<>(first(gamma));
                        if (R.remove(EPS)) {
                            R.addAll(FOLLOW.get(A));
                        }

                        if (FOLLOW.get(B).addAll(R)) {
                            flag = true;
                        }
                    }
                }
            }
            if (!flag) {
                break;
            }
        }
    }

    private boolean isLL1() {
        for (var r : rules.entrySet()) {
            String A = r.getKey();
            for (var a : getAllTrans(r)) {
                for (var b : getAllTrans(r)) {
                    if (a == b) {
                        continue;
                    }
                    if (checkNonEmptyIntersection(first(a), first(b))) {
                        return false;
                    }
                    if (first(a).contains(EPS)) {
                        if (checkNonEmptyIntersection(first(b), FOLLOW.get(A))) {
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }

    private boolean checkNonEmptyIntersection(Set<String> a, Set<String> b) {
        a.retainAll(b);
        return !a.isEmpty();
    }
//////////////////////////////////////////////////////


    private void generateStartAndImports() throws IOException {
        writer.write("""
                import java.io.BufferedWriter
                import java.io.Reader
                import java.text.ParseException
                import kotlin.jvm.Throws""");
        writer.write("\n\n");

        generateTreeClass();

        writer.write("""
                class Parser {
                    private val lex = Lexer()
                    
                    private fun token() : Token {
                        return lex.curToken()
                    }
                    
                    @Throws(ParseException::class)
                    fun parse(input : Reader) : Pair<%s, Tree> {
                        lex.setInput(input)
                        lex.nextToken()
                        var result = `%s`()
                        if (token().terminal != Terminal.`$`) {
                            throw createParseException(Terminal.`$`)
                        }
                        return result
                    }
                    
                    private fun createParseException(vararg tokens : Terminal) : ParseException {
                        var enumerationStr = ""
                        for (i in tokens.indices) {
                            enumerationStr += tokens[i]
                            if (i < tokens.size - 1) {
                                enumerationStr += " or "
                            }
                        }
                        return ParseException(
                            "$enumerationStr expected, but was ${lex.curToken().terminal} as text: ${lex.curToken().textVal} at position ",
                            lex.curPos()
                        )
                    }
                    
                    private fun ensure(x : Terminal) : String {
                        if (token().terminal == x) {
                            val t = token().textVal
                            lex.nextToken()
                            return t
                        } else {
                            throw createParseException(x)
                        }
                    }
                    
                """.formatted(signatures.get(startTerminal).getKey(), startTerminal));
    }

    private void generateTreeClass() throws IOException {
        writer.write("""
                class Tree (val node : String, vararg children : Tree) {
                                
                    companion object {
                        var globalNumber = 0
                    }
                                
                    var children : ArrayList<Tree>
                    private val number = globalNumber++
                                
                    init {
                        this.children = ArrayList(children.toList())
                    }
                                
                    constructor(node: String) : this(node, *emptyArray())
                                
                    constructor(nodeType : Terminal) : this(nodeType.name)
                                
                    fun addChild(child : Tree) {
                        children.add(child)
                    }
                                
                    fun <T> addChild(pChild : Pair<T, Tree>) : T {
                        children.add(pChild.second)
                        return pChild.first
                    }
                                
                    fun toDot(out : BufferedWriter) {
                        if (node == "S") {
                            childrenToDot(out)
                        } else {
                            childrenToDot(out)
                        }
                    }
                                
                    private fun childrenToDot(out : BufferedWriter) {
                        out.write("\\"${node + number}\\"[label=\\"${node}\\"]")
                        out.newLine()
                        for (ch in children) {
                            out.write("\\"${node + number}\\" -> \\"${ch.node + ch.number}\\";")
                            out.newLine()
                        }
                        for (ch in children) {
                            ch.toDot(out)
                        }
                    }
                }
                                
                                
                """);
        writer.write("\n\n");
    }

    private void generateEnd() throws IOException {
        writer.write("}\n");

        writer.close();
    }

    private void generateNonTerminalFunction(String A) throws IOException {
        writer.write("""
                
                    private fun `%s`(%s): Pair<%s, Tree> {
                        var r = Tree("%s")
                        val result = when(token().terminal) {
                """.formatted(A, signatures.get(A).getValue(), signatures.get(A).getKey(), A));

        Set<String> args = new HashSet<>();
        for (var alpha : rules.get(A)) {
            Set<String> fst1 = first1(A, alpha.trans());
            args.addAll(fst1);
            writer.write(
                    "%s%s -> {%s\n%s}\n".formatted(
                            TAB.repeat(3),
                            formatSetTokens(fst1),
                            createCase(alpha.trans(), alpha.returnCode(), alpha.listArgCode(), A),
                            TAB.repeat(3)
                    )
            );
        }


        writer.write("\n%selse -> throw createParseException(%s)\n".formatted(
                TAB.repeat(3),
                formatSetTokens(args)
        ));


        writer.write("""
                        }
                        return Pair(result, r)
                    }
                """);
    }

    private Set<String> first1(String A, List<String> alpha) {
        Set<String> res = first(alpha);
        if (res.remove(EPS)) {
            res.addAll(FOLLOW.get(A));
        }
        return res;
    }

    private String formatSetTokens(Set<String> firsts) {
        return firsts.stream().map("Terminal.`%s`"::formatted).collect(Collectors.joining(", "));
    }

    private String createCase(List<String> alpha, Optional<String> attributeReturnCode, List<String> argCalcCodes, String nonTerminal) {
        StringBuilder res = new StringBuilder("\n");

        Map<String, Integer> nameToCounts = new HashMap<>();

        for (int i = 0; i < alpha.size(); i++) {
            String X = alpha.get(i);
            String num;
            if (nameToCounts.containsKey(X)) {
                num = Integer.toString(nameToCounts.get(X));
                nameToCounts.put(X, nameToCounts.get(X) + 1);
            } else {
                num = "";
                nameToCounts.put(X, 1);
            }

            if (isTerminal(X)) {
                if (X.equals(EPS)) {
                    continue;
                }

                res.append("""
                                        val `%s` = ensure(Terminal.%s)
                                        r.addChild(Tree(Terminal.%s))
                        """.formatted(X + num, X, X));
            } else {
                //Наследуемый атрибут s()
                if (!signatures.get(X).getValue().equals(EMPTY_ARG) && argCalcCodes.get(i).equals(EMPTY_ARG)) {
                    throw new ParserGenException(
                            "NonTerminal %s contains inherited attribute but definition in rule is empty.".formatted(X)
                    );
                }
                res.append("""
                                        val `%s` = r.addChild(`%s`(%s))
                        """.formatted(X + num, X, argCalcCodes.get(i)));
            }
        }

        // Синтезируемый атрибут (return ...)
        if (!signatures.get(nonTerminal).getKey().equals(EMPTY_RETURN)) {
            if (attributeReturnCode.isPresent()) {
                res.append(TAB.repeat(4)).append(attributeReturnCode.get());
            } else {
                throw new ParserGenException(
                        "NonTerminal %s contains synthesized attribute but definition in rule is empty".formatted(nonTerminal)
                );
            }
        } else {
            res.append("%snull".formatted(TAB.repeat(4)));
        }

        return res.toString();
    }


}
