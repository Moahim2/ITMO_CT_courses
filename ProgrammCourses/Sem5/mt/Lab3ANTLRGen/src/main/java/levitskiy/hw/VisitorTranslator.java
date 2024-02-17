package levitskiy.hw;

import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;


public class VisitorTranslator extends levitskiy.hw.PrefixLanguageBaseVisitor<String> {
    private static final String WS = " ";
    private static final String TAB = " ".repeat(4);
    private static final String NEW_LINE = System.lineSeparator();

    private final Set<String> allVariables = new HashSet<>();
    private final Map<String, Type> variablesInType = new HashMap<>();

    private int curCountOfTab = 0;
    private Type lastOperationType;
    
    @Override
    public String visitTerminal(TerminalNode node) {
        String value = node.getText();
        return switch (node.getSymbol().getType()) {
            case PrefixLanguageParser.EOF -> "";
            case PrefixLanguageParser.TRUE -> "true";
            case PrefixLanguageParser.FALSE -> "false";
            case PrefixLanguageParser.VAR_NAME -> {
                allVariables.add(value);
                yield value;
            }
            default -> value;
        };
    }

    @Override
    public String visitProgram(levitskiy.hw.PrefixLanguageParser.ProgramContext ctx) {
        String program = visitChildren(ctx);

        String declareAllVars = "";
        for (String variable : allVariables) {
            Type type = variablesInType.get(variable);
            if (type == null) {
                throw new TranslationException("Variable without type!");
            }

            declareAllVars = "%s%s%svar %s = %s".formatted(
                    declareAllVars, NEW_LINE, TAB, variable, type.getDefaultValue()
            );
        }
        declareAllVars += NEW_LINE;

        return "fun main() { %s%s%s}".formatted(
                NEW_LINE,
                declareAllVars,
                program
        );
    }

    @Override
    public String visitOperation(levitskiy.hw.PrefixLanguageParser.OperationContext ctx) {
        ++curCountOfTab;
        return addTabulation(visitChildren(ctx)) + NEW_LINE;
    }

    @Override
    public String visitArithmetic(levitskiy.hw.PrefixLanguageParser.ArithmeticContext ctx) {
        String op = visitChild(ctx, 0);
        String result =  switch (ctx.getChildCount()) {
            case 3 -> "(%s)%s%s%s(%s)".formatted(
                    visitChild(ctx, 1),
                    WS,
                    op,
                    WS,
                    visitChild(ctx, 2)
            );
            case 2 -> op + "(" + visitChild(ctx, 1) + ")";
            case 1 -> op;
            default -> null;
        };
        lastOperationType = Type.Int;
        return result;
    }

    @Override
    public String visitLogic(levitskiy.hw.PrefixLanguageParser.LogicContext ctx) {
        String op = visitChild(ctx, 0);
        String result = switch (ctx.getChildCount()) {
            case 3 -> visitChild(ctx, 1) + WS + op + WS + visitChild(ctx, 2);
            case 2 -> op + "(" + visitChild(ctx, 1) + ")";
            case 1 -> op;
            default -> null;
        };
        lastOperationType = Type.Boolean;
        return result;
    }

    @Override
    public String visitLogicBranch(levitskiy.hw.PrefixLanguageParser.LogicBranchContext ctx) {
        return "if (%s) {%s%s%s}".formatted(
                visitChild(ctx, 1),
                NEW_LINE,
                visitChild(ctx, 2),
                TAB.repeat(curCountOfTab)
        );
    }

    @Override
    public String visitPrint(levitskiy.hw.PrefixLanguageParser.PrintContext ctx) {
        return "System.out.print(%s)".formatted(visitChild(ctx, 1));
    }

    @Override
    public String visitAssignment(levitskiy.hw.PrefixLanguageParser.AssignmentContext ctx) {
        String name = visitChild(ctx, 1);
        String val = visitChild(ctx, 2);

        Type lastType = variablesInType.get(name);
        if (lastType != null && !lastType.equals(lastOperationType)) {
            throw new TranslationException("Assigning the wrong type!");
        }

        Type varType = variablesInType.get(val);
        if (varType != null) {
            lastOperationType = varType;
        }
        variablesInType.put(name, lastOperationType);

        return "%s%s=%s%s".formatted(
                name,
                WS,
                WS,
                val
        );
    }

    @Override
    public String visitWhileCycle(levitskiy.hw.PrefixLanguageParser.WhileCycleContext ctx) {
        StringBuilder operations = new StringBuilder();
        for (int i = 4; i < ctx.getChildCount() - 1; i++) {
            operations.append(visitChild(ctx, i));
        }
        return "while (%s) {%s%s%s}".formatted(
                visitChild(ctx, 1),
                NEW_LINE,
                operations.toString(),
                TAB.repeat(curCountOfTab)
        );
    }

    @Override
    protected String aggregateResult(String aggregate, String nextResult) {
        return aggregate + nextResult;
    }

    @Override
    protected String defaultResult() {
        return "";
    }

    private String visitChild(ParseTree parseTree, int numberOfChild) {
        return visit(parseTree.getChild(numberOfChild));
    }

    private String addTabulation(String string) {
        return TAB.repeat(curCountOfTab--) + string;
    }
}
