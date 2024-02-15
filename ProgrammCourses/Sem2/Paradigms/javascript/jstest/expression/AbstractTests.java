package jstest.expression;

import base.ExtendedRandom;
import jstest.expression.BaseTester.Expr;
import jstest.expression.BaseTester.Func;

import java.util.*;
import java.util.function.DoubleBinaryOperator;
import java.util.function.DoubleUnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public abstract class AbstractTests {
    private static final int MAX_C = 1_000;

    private final ExtendedRandom random = new ExtendedRandom(getClass());

    public final List<TestExpression> tests = new ArrayList<>();
    private final Map<String, Operator> operators = new HashMap<>();
    private final Map<String, Func> nullary = new HashMap<>();
    private final List<TestExpression> variables = new ArrayList<>();

    private final Map<String, Integer> variableNames = new HashMap<>();
    private final List<String> operatorNames = new ArrayList<>();
    private final List<String> nullaryNames = new ArrayList<>();
    private final Map<String, Integer> priorities = new HashMap<>();
    private final List<int[]> simplifications = new ArrayList<>();

    public List<Expr> renderTests(final Dialect parsed, final Dialect unparsed) {
        return tests.stream().map(test -> test.render(parsed, unparsed)).collect(Collectors.toList());
    }

    public Expr randomTest(final int size, final Dialect parsed, final Dialect unparsed) {
        return generate(size / 10 + 2).render(parsed, unparsed);
    }

    private TestExpression generate(final int depth) {
        if (depth > 0) {
            return generateOp(depth);
        } else if (random().nextBoolean()) {
            return random().randomItem(variables);
        } else if (nullary.isEmpty() || random().nextBoolean()){
            return c(random().nextInt(-MAX_C, MAX_C));
        } else {
            return n(random().randomItem(nullaryNames));
        }
    }

    protected TestExpression generateOp(final int depth) {
        if (random().nextInt(6) == 0 || operatorNames.isEmpty()) {
            return generateP(depth);
        } else {
            final String name = random.randomItem(operatorNames);
            final Operator operator = operators.get(name);
            final TestExpression[] args = Stream.generate(() -> generateP(depth))
                    .limit(random().nextInt(operator.minArity, operator.maxArity))
                    .toArray(TestExpression[]::new);
            return f(name, args);
        }
    }

    protected TestExpression generateP(final int depth) {
        return generate(random().nextInt(depth));
    }

    public void tests(final TestExpression... tests) {
        this.tests.addAll(Arrays.asList(tests));
    }

    public void tests(final int[][] simplifications, final TestExpression... tests) {
        if (simplifications == null) {
            this.simplifications.addAll(Collections.nCopies(tests.length, null));
        } else {
            assert simplifications.length == tests.length;
            this.simplifications.addAll(Arrays.asList(simplifications));
        }
        tests(tests);
    }

    public void fixed(final String name, final int arity, final Func f) {
        any(name, arity, arity, f);
    }

    public void any(final String name, final int minArity, final int maxArity, final Func f) {
        operatorNames.add(name);
        operators.put(name, new Operator(minArity, maxArity, f));
    }

    public void unary(final String name, final DoubleUnaryOperator answer) {
        fixed(name, 1, args -> answer.applyAsDouble(args[0]));
    }

    public void binary(final String name, final DoubleBinaryOperator answer) {
        fixed(name, 2, args -> answer.applyAsDouble(args[0], args[1]));
    }

    public void infix(final String name, final int priority, final DoubleBinaryOperator answer) {
        binary(name, answer);
        priorities.put(name, priority);
    }

    public void nullary(final String name, final Func f) {
        nullaryNames.add(name);
        nullary.put(name, f);
    }

    public TestExpression f(final String name, final TestExpression... args) {
        Arrays.stream(args).forEach(Objects::requireNonNull);
        return (parsed, unparsed) -> {
            final List<Expr> rendered = Arrays.stream(args).map(a -> a.render(parsed, unparsed)).collect(Collectors.toList());
            return expr(
                    parsed.operation(name, rendered.stream().map(a -> a.parsed).collect(Collectors.toList())),
                    unparsed.operation(name, rendered.stream().map(a -> a.unparsed).collect(Collectors.toList())),
                    vars -> {
                        final Operator operator = operators.get(name);
                        assert operator != null : "Unknown operation " + name + " in " + getClass().getSimpleName();
                        return operator.applyAsDouble(rendered.stream()
                                .map(a -> a.answer)
                                .mapToDouble(a -> a.applyAsDouble(vars))
                                .toArray());
                    }
            );
        };
    }

    protected TestExpression n(final String name) {
        return (parsed, unparsed) -> expr(Dialect.nullary(name), Dialect.nullary(name), nullary.get(name));
    }

    public static TestExpression c(final int value) {
        return (parsed, unparsed) -> expr(parsed.constant(value), unparsed.constant(value), vars -> value);
    }

    public TestExpression variable(final String name, final int index) {
        final TestExpression variable = (parsed, unparsed) -> expr(parsed.variable(name), unparsed.variable(name), vars -> vars[index]);
        variables.add(variable);
        variableNames.put(name, index);
        return variable;
    }

    public static Expr expr(final String parsed, final String unparsed, final Func answer) {
        return new Expr(parsed, unparsed, answer);
    }

    public List<TestExpression> getVariables() {
        return variables;
    }

    public Map<String, Integer> getVariableNames() {
        return variableNames;
    }

    public ExtendedRandom random() {
        return random;
    }

    public boolean hasVarargs() {
        return operators.values().stream().anyMatch(Operator::isVararg);
    }

    public Integer getPriority(final String op) {
        return priorities.get(op);
    }

    public List<int[]> getSimplifications() {
        return simplifications;
    }

    public interface TestExpression {
        Expr render(Dialect parsed, Dialect unparsed);
    }

    private static class Operator implements Func {
        public final int minArity, maxArity;
        public Func f;

        public Operator(final int minArity, final int maxArity, final Func f) {
            assert 0 <= minArity && minArity <= maxArity;

            this.minArity = minArity;
            this.maxArity = maxArity;
            this.f = f;
        }

        @Override
        public double applyAsDouble(final double[] args) {
            return Arrays.stream(args).allMatch(Double::isFinite) ? f.applyAsDouble(args) : Double.NaN;
        }

        public boolean isVararg() {
            return minArity < maxArity;
        }
    }
}
