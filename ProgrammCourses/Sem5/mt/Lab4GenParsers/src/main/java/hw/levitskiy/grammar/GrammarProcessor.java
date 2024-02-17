package hw.levitskiy.grammar;

import hw.levitskiy.GPRuleLanguageLexer;
import hw.levitskiy.GPRuleLanguageParser;
import hw.levitskiy.parser.RuleRightPart;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static hw.levitskiy.util.Util.*;

public class GrammarProcessor {
    private final GPRuleLanguageParser.GrContext grammar;


    public GrammarProcessor(CharStream inputGrammar) {
        GPRuleLanguageLexer lexer = new GPRuleLanguageLexer(inputGrammar);
        GPRuleLanguageParser parser = new GPRuleLanguageParser(new CommonTokenStream(lexer));

        grammar = parser.gr();
    }

    public Map<String, Set<RuleRightPart>> getRules() {
        return grammar.rule_bijection().stream().collect(Collectors.toMap(
                t -> t.TOKEN_NAME().getText(),
                t -> new HashSet<>(Set.of(new RuleRightPart(getRuleTList(t), getRuleReturn(t), getRuleArgCalculatedCode(t)))),
                (o, n) -> {
                    o.addAll(n);
                    return o;
                }
        ));
    }


    public String getPackage() {
        return getRegVal(grammar.sub_package(0).TOKEN());
    }

    public String getSkip() {
        return grammar.skip().stream().map(t -> t.TOKEN().getText()).collect(Collectors.joining("|"));
    }

    public String getStartTerminal() {
        return grammar.start_terminal(0).TOKEN_NAME().getText();
    }

    public Map<String, String> getTokens() {
        return grammar.token_bijection().stream().collect(Collectors.toMap(
                t -> t.TOKEN_NAME().getText(),
                t -> getRegVal(t.TOKEN())
        ));
    }

    public Map<String, Map.Entry<String, String>> getSignature() {
        return grammar.return_val().stream().collect(Collectors.toMap(
                t -> t.TOKEN_NAME().getText(),
                t -> Map.entry(getRegVal(t.TOKEN()), getRuleArgument(t))
        ));
    }

    private static String getRegVal(TerminalNode t) {
        String reg = t.getText();
        return reg.substring(1, reg.length() - 1);
    }

    private static String getRegVal2(TerminalNode t) {
        String reg = t.getText();
        return reg.substring(2, reg.length() - 2);
    }

    private List<String> getRuleTList(GPRuleLanguageParser.Rule_bijectionContext t) {
        if (t.EPS() != null) {
            return List.of(EPS);
        }
        return t.def_tr().stream().map(i -> i.TOKEN_NAME().getText()).toList();
    }

    private Optional<String> getRuleReturn(GPRuleLanguageParser.Rule_bijectionContext t) {
        if (t.TOKEN() != null) {
            return Optional.of(getRegVal(t.TOKEN()));
        }
        return Optional.empty();
    }

    private String getRuleArgument(GPRuleLanguageParser.Return_valContext t) {
        if (t.BR_ATTR_RULE() != null) {
            return getRegVal2(t.BR_ATTR_RULE());
        } else {
            return EMPTY_ARG;
        }
    }

    private List<String> getRuleArgCalculatedCode(GPRuleLanguageParser.Rule_bijectionContext t) {
        List<String> result = new ArrayList<>();
        if (t.def_tr() == null || t.def_tr().isEmpty()) {
            return List.of(EMPTY_ARG);
        } else {
            for (var ch : t.def_tr()) {
                if (ch.BR_ATTR_RULE() == null) {
                    result.add(EMPTY_ARG);
                } else {
                    result.add(getRegVal2(ch.BR_ATTR_RULE()));
                }
            }
        }
        return result;
    }

}
