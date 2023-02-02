package expression.parser;

import expression.*;
import expression.exceptions.TripleParser;

public final class ExpressionParser implements TripleParser {

    public TripleExpression parse(String expression) {
        return parse(new StringCharSourse(expression));
    }

    public static TripleExpression parse(final CharSourse sourse) {
        return new ExpressionPars(sourse).parse();
    }

    private static class ExpressionPars extends BaseParser {
        private TripleExpression parsingExpr;
        private boolean startingMinus = true;
        private boolean parsingThirdOperation = false;
        private boolean parsingFourthOperation = false;

        public ExpressionPars(CharSourse sourse) {
            super(sourse);
        }

        public TripleExpression parse() {
            TripleExpression result = parseElement();
            while (!end()) {
                result = parseElement();
            }
            return result;
        }

        private TripleExpression parseElement() {
            skipWhitespace();
            final TripleExpression result = parseValue();
            skipWhitespace();
            return result;
        }

        private TripleExpression parseValue() {
            if (take('l')) {
                expect('0');
                parsingExpr = parseUnaryOperation(0);
                return parsingExpr;
            }
            if (take('t')) {
                expect('0');
                parsingExpr = parseUnaryOperation(1);
                return parsingExpr;
            }
            if (between('0', '9')) {
                parsingExpr = parseConst(0);
                return parsingExpr;
            }
            if (current('x') || current('y') || current('z')) {
                parsingExpr = parseVariable();
                return parsingExpr;
            }
            if (current('-')) {
                skipWhitespace();
                if (startingMinus) { //обработка стартового унарного минуса
                    take();
                    parsingExpr = parseUnaryMinus();
                } else {
                    if (parsingThirdOperation) {
                        parsingThirdOperation = false;
                        return parsingExpr;
                    }
                    take();
                    parsingExpr = parseThirdOperation('-');
                }
                return parsingExpr;
            }
            if (current('+')) {
                if (parsingThirdOperation) {
                    parsingThirdOperation = false;
                    return parsingExpr;
                }
                take();
                parsingExpr = parseThirdOperation('+');
                return parsingExpr;
            }

            if (current('*') || current('/')) {
                parsingExpr = parseSecondOperation();
                return parsingExpr;
            }
            if (take('(')) {
                parsingExpr = parseBracket();
                return parsingExpr;
            }
            if (current(')')) {
                if (parsingThirdOperation) {
                    parsingThirdOperation = false;
                }
                if (parsingFourthOperation) {
                    parsingFourthOperation = false;
                }
                return parsingExpr;
            }
            if (current('m')) {
                if (parsingThirdOperation) {
                    parsingThirdOperation = false;
                    return parsingExpr;
                }
                if (parsingFourthOperation) {
                    parsingFourthOperation = false;
                    return parsingExpr;
                }
                take();
                if (take('i')) {
                    expect('n');
                    parsingExpr = parseFourthOperation(0);
                    return parsingExpr;
                } else if (take('a')) {
                    expect('x');
                    parsingExpr = parseFourthOperation(1);
                    return parsingExpr;
                }
            }
            if (current('\0') && (parsingThirdOperation || parsingFourthOperation)) {
                parsingThirdOperation = false;
                parsingFourthOperation = false;
                return parsingExpr;
            }
            throw error("Invalid value in area: " );
        }

        private void skipWhitespace() {
            while (isWhitespace()) {
                take();
            }
        }

        private boolean currentIsParsTern() {
            skipWhitespace();
            return Character.isDigit(current()) || current('(') || current(')') ||
                    current('x') || current('y') || current('z') ||
                    current('l') || current('t');
        }

        private Const parseConst(int minus) {
            startingMinus = false;
            final StringBuilder number = new StringBuilder();
            if (take('-')) {
                number.append('-');
            }
            while (between('0', '9')) {
                number.append(take());
            }
            try {
                if (minus == 0) {
                    return new Const(Integer.parseInt(number.toString()));
                } else {
                    return new Const(Integer.parseInt("-" + number));
                }
            } catch (NumberFormatException e) {
                throw error("Invalid number: " + number + ": " + e.getMessage());
            }
        }

        private Variable parseVariable() {
            startingMinus = false;
            final String variable = Character.toString(take());
            if (current('+') || current('-') || current('*') || current('/') ||
                    current('(') || current(')') ||Character.isWhitespace(current()) ||
                    current('\0') || current('m')) {
                return new Variable(variable);
            } else {
                throw error("Invalid Variable: " + variable + current());
            }
        }

        private TripleExpression parseUnaryMinus() {
            startingMinus = false;
            boolean skippinngWhitespace = false;

            if (Character.isWhitespace(current())) {
                skipWhitespace();
                skippinngWhitespace = true;
            }

            if (current('(') || current('l') || current('t')) {
                return new UnaryMinus(parseElement());
            } else if (current('x') || current('y') || current('z')) {
                return new UnaryMinus(new Variable(Character.toString(take())));
            } else if (Character.isDigit(current())) {
                if (skippinngWhitespace) {
                    return new UnaryMinus(parseConst(0));
                } else {
                    return parseConst(1);
                }
            } else if (take('-')) {
                return new UnaryMinus(parseUnaryMinus());
            }
            throw error("Invalid UnariMinus: ");
        }

        private TripleExpression parseThirdOperation(char type) {
            parsingThirdOperation = true;
            startingMinus = true;
            TripleExpression thirdOperation = null;
            TripleExpression lastParsing = parsingExpr;

            if (type == '+') {
                if (currentIsParsTern() || current('-')) {
                    while (parsingThirdOperation) {
                        thirdOperation = parseElement();
                    }
                    startingMinus = false;
                    return new Add(lastParsing, thirdOperation);
                }
            } else if (type == '-') {
                if (currentIsParsTern() || current('-')) {
                    while (parsingThirdOperation) {
                        thirdOperation = parseElement();
                    }
                    startingMinus = false;
                    return new Subtract(lastParsing, thirdOperation);
                }
            }
            throw error("Invalid operation: ");
        }

        private TripleExpression parseSecondOperation() {
            startingMinus = false;

            if (take('*')) {
                if (currentIsParsTern()) {
                    return new Multiply(parsingExpr, parseElement());
                } else if (take('-')) {
                    return new Multiply(parsingExpr, parseUnaryMinus());
                }
            } else if (take('/')) {
                if (currentIsParsTern()) {
                    return new Divide(parsingExpr, parseElement());
                } else if (take('-')) {
                    return new Divide(parsingExpr, parseUnaryMinus());
                }
            }
            throw error("Invalid operation: ");
        }

        private TripleExpression parseFourthOperation(int type) {
            parsingFourthOperation = true;
            startingMinus = true;
            TripleExpression fourthOperation = null;
            TripleExpression lastParsing = parsingExpr;

            if (type == 0) {
                if (currentIsParsTern() || current('-')) {
                    while (parsingFourthOperation) {
                        fourthOperation = parseElement();
                    }
                    startingMinus = false;
                    return new Min(lastParsing, fourthOperation);
                }
            } else if (type == 1) {
                if (currentIsParsTern() || current('-')) {
                    while (parsingFourthOperation) {
                        fourthOperation = parseElement();
                    }
                    startingMinus = false;
                    return new Max(lastParsing, fourthOperation);
                }
            }
            throw error("Invalid operation: ");
        }
        private TripleExpression parseUnaryOperation(int type) {
            startingMinus = true;
            skipWhitespace();
            if (type == 0) {
                return new L0(parseElement());
            } else {
                return new T0(parseElement());
            }
        }
        private TripleExpression parseBracket() {
            TripleExpression bracket = null;

            startingMinus = true;
            boolean bracketParsingThirdOperation = parsingThirdOperation;
            boolean bracketParsingFourthOperation = parsingFourthOperation;
            while (!take(')')) {
                if (end()) {
                    throw error("Incorrectly bracketOperation: ");
                }
                bracket = parseElement();
            }
            parsingThirdOperation = bracketParsingThirdOperation;
            parsingFourthOperation = bracketParsingFourthOperation;
            startingMinus = false;
            return bracket;
        }
    }
}
