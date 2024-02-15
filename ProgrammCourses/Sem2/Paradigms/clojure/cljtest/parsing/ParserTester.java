package cljtest.parsing;

import base.ExtendedRandom;
import base.Selector;
import cljtest.functional.FunctionalTester;
import cljtest.object.ObjectTester;
import jstest.expression.AbstractTests;
import jstest.expression.Builder;
import jstest.expression.Dialect;

import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.IntPredicate;
import java.util.function.ToIntFunction;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class ParserTester {
    private ParserTester() {
    }

    /* package-private*/ static Selector.Composite<Builder> builder() {
        return Builder.selector(
                ParserTester.class,
                mode -> false,
                (builder, counter) -> {
                    final Mode mode = counter.mode() == 1 ? Mode.INFIX : Mode.SUFFIX;
                    return new ObjectTester(
                            counter,
                            builder.aliased(ObjectTester.PARSED, mode.unparsed),
                            false,
                            mode.parse,
                            mode.toString,
                            spoiler(mode, builder.getTests())
                    );
                },
                "easy", "hard"
        );
    }

    private static BiFunction<ExtendedRandom, String, String> spoiler(final Mode mode, final AbstractTests tests) {
        return (random, expression) -> {
            if (random.nextBoolean()) {
                return expression;
            }
            if (mode == Mode.INFIX) {
                final Parsed parsed = new Parser(expression, tests.getVariableNames().keySet(), tests::getPriority).parse();
                expression = parsed.convert(new StringBuilder(), 0).toString();
            }
            return ObjectTester.addSpaces(expression, random);
        };
    }

    private enum Mode {
        SUFFIX("parseObjectSuffix", "toStringSuffix", new Dialect("%s", "%s.0", "({args} {op})", " " )),
        INFIX("parseObjectInfix", "toStringInfix", new Dialect(
                "%s",
                "%s.0",
                (op, args) -> {
                    switch (args.size()) {
                        case 1: return op + "(" + args.get(0) + ")";
                        case 2: return "(" + args.get(0) + " " + op + " " + args.get(1) + ")";
                        default: throw new AssertionError("Unsupported op " + op + "/" + args.size());
                    }
                }
        ));

        private final String parse;
        private final String toString;
        private final Dialect unparsed;

        Mode(final String parse, final String toString, final Dialect unparsed) {
            this.unparsed = unparsed;
            this.toString = toString;
            this.parse = parse;
        }
    }

    private interface Parsed {
        StringBuilder convert(StringBuilder sb, int priority);
    }

    private static class Parser {
        private final String expression;
        private final Set<String> vars;
        private final ToIntFunction<String> priorities;
        int pos = 0;

        public Parser(final String expression, final Set<String> vars, final ToIntFunction<String> priorities) {
            this.expression = expression + "$";
            this.vars = vars;
            this.priorities = priorities;
        }

        public Parsed parse() {
            skipSpaces();
            if (test('(')) {
                final Parsed left = parse();
                skipSpaces();
                final String op = parseIdentifier();
                final Parsed right = parse();
                skipSpaces();
                expect(')');
                return (sb, priority) -> {
                    final int p = priorities.applyAsInt(op);
                    final int local = Math.abs(p);
                    final int l = local + (p > 0 ? 0 : 1);
                    final int r = local + (p > 0 ? 1 : 0);
                    if (local < priority) {
                        return right.convert(left.convert(sb.append("("), l).append(op), r).append(")");
                    } else {
                        return right.convert(left.convert(sb, l).append(op), r);
                    }
                };
            } else if (Character.isDigit(getChar()) || getChar() == '-') {
                final char first = getChar();
                pos++;
                final String value = first + get(ch -> Character.isDigit(ch) || ch == '.');
                return (sb, priority) -> sb.append(value);
            } else {
                final String identifier = parseIdentifier();
                if (vars.contains(identifier)) {
                    return (sb, priority) -> sb.append(identifier);
                } else {
                    skipSpaces();
                    expect('(');
                    final Parsed arg = parse();
                    skipSpaces();
                    expect(')');
                    return (sb, priority) -> arg.convert(sb.append(identifier).append(" "), Integer.MAX_VALUE);
                }
            }
        }

        private static final String SYMBOLS = "*/-+&|^<>=";

        private String parseIdentifier() {
            final char first = getChar();
            if (Character.isLetter(first)) {
                return get(Character::isLetterOrDigit);
            } else {
                return get(ch -> SYMBOLS.indexOf(ch) >= 0);
            }
        }

        private void expect(final char ch) {
            if (!test(ch)) {
                throw new AssertionError(String.format("%d: expected '%c', found '%c'", pos + 1, ch, getChar()));
            }
        }

        private char getChar() {
            return expression.charAt(pos);
        }

        private boolean test(final char ch) {
            if (getChar() == ch) {
                pos++;
                return true;
            }
            return false;
        }

        private void skipSpaces() {
            get(Character::isWhitespace);
        }

        private String get(final IntPredicate p) {
            final int start = pos;
            while (p.test(getChar())) {
                pos++;
            }
            return expression.substring(start, pos);
        }
    }
}
