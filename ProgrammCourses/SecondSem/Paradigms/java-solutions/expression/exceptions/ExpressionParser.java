package expression.exceptions;

import expression.*;

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
        private boolean parsingVarOrConst = true;

        public ExpressionPars(CharSourse source) {
            super(source);
        }

        public TripleExpression parse() {
            try {
                TripleExpression result = parseElement();
                while (!end()) {
                    result = parseElement();
                }
                return result;
            } catch (ArithmeticException e) {
                System.out.println(e.getMessage());
                return null;
            } catch (IllegalArgumentException e) {
                throw new IllegalArgumentException(e.getMessage());
            }
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
                if (!Character.isWhitespace(current()) && !current('(')) {
                    throw error("Parsing error, missing whitespace: " + errorBuffer);
                }
                parsingExpr = parseUnaryOperation(0);
                return parsingExpr;
            }
            if (take('t')) {
                expect('0');
                if (!Character.isWhitespace(current()) && !current('(')) {
                    throw error("Parsing error, missing whitespace: " + errorBuffer);
                }
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
                    if (parsingVarOrConst) {
                        throw error("Parsing error, missing sign: " + errorBuffer);
                    }
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
                if (parsingVarOrConst) {
                    throw error("Parsing error, missing sign: " + errorBuffer);
                }
                if (parsingThirdOperation) {
                    parsingThirdOperation = false;
                    return parsingExpr;
                }
                take();
                parsingExpr = parseThirdOperation('+');
                return parsingExpr;
            }

            if (current('*') || current('/')) {
                if (parsingVarOrConst) {
                    throw error("Parsing error, missing sign: " + errorBuffer);
                }
                parsingExpr = parseSecondOperation();
                return parsingExpr;
            }
            if (take('(')) {
                parsingExpr = parseBracket();
                return parsingExpr;
            }
            if (current(')')) {
                if (parsingVarOrConst) {
                    throw error("Parsing error, missing argument in area: " + errorBuffer);
                }
                if (!parsingFourthOperation && !parsingThirdOperation) {
                    throw error("Incorrectly bracketOperation in area: " + errorBuffer);
                }
                if (parsingThirdOperation) {
                    parsingThirdOperation = false;
                }
                if (parsingFourthOperation) {
                    parsingFourthOperation = false;
                }
                return parsingExpr;
            }
            if (current('m')) {
                if (!Character.isWhitespace(errorBuffer.charAt(errorBuffer.length() - 1)) &&
                        errorBuffer.charAt(errorBuffer.length() - 1) != ')' &&
                        errorBuffer.charAt(errorBuffer.length() - 1) != 'x' &&
                        errorBuffer.charAt(errorBuffer.length() - 1) != 'y' &&
                        errorBuffer.charAt(errorBuffer.length() - 1) !='z') {
                    throw error("Parsing error, missing whitespace: " + errorBuffer);
                }
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
                    if (!Character.isWhitespace(current()) && !current('(') && !current('-')
                            && !current('t') && !current('l') && !current('x') &&
                            !current('y') && !current('z')) {
                        throw error("Parsing error, missing whitespace: " + errorBuffer);
                    }
                    parsingExpr = parseFourthOperation(0);
                    return parsingExpr;
                } else if (take('a')) {
                    expect('x');
                    if (!Character.isWhitespace(current()) && !current('(') && !current('-')
                            && !current('t') && !current('l') && !current('x') &&
                            !current('y') && !current('z')) {
                        throw error("Parsing error, missing whitespace: " + errorBuffer);
                    }
                    parsingExpr = parseFourthOperation(1);
                    return parsingExpr;
                }
            }
            if (current('\0') && (parsingThirdOperation || parsingFourthOperation)) {
                parsingThirdOperation = false;
                parsingFourthOperation = false;
                return parsingExpr;
            }
            throw error("Invalid value in area: " + errorBuffer);
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
            if (!parsingVarOrConst) {
                throw error("Parsing error, missing sign between arguments: " + errorBuffer);
            }
            startingMinus = false;
            final StringBuilder number = new StringBuilder();
            if (take('-')) {
                number.append('-');
            }
            while (between('0', '9')) {
                number.append(take());
            }
            try {
                parsingVarOrConst = false;
                if (minus == 0) {
                    return new Const(Integer.parseInt(number.toString()));
                } else {
                    return new Const(Integer.parseInt("-" + number));
                }
            } catch (NumberFormatException e) {
                throw error("Invalid number: " + number + ": " + e.getMessage() + " in area " + errorBuffer);
            }
        }

        private Variable parseVariable() {
            if (!parsingVarOrConst) {
                throw error("Parsing error, missing sign between arguments: " + errorBuffer);
            }
            startingMinus = false;
            final String variable = Character.toString(take());
            if (current('+') || current('-') || current('*') || current('/') ||
                    current('(') || current(')') ||Character.isWhitespace(current()) ||
                    current('\0') || current('m')) {
                parsingVarOrConst = false;
                return new Variable(variable);
            } else {
                throw error("Invalid Variable in area: " + errorBuffer);
            }
        }

        private TripleExpression parseUnaryMinus() {
            parsingVarOrConst = true;
            startingMinus = false;
            boolean skippinngWhitespace = false;

            if (Character.isWhitespace(current())) {
                skipWhitespace();
                skippinngWhitespace = true;
            }

            if (current('(') || current('l') || current('t')) {
                return new CheckedNegate(parseElement());
            } else if (current('x') || current('y') || current('z')) {
                parsingVarOrConst = false;
                return new CheckedNegate(new Variable(Character.toString(take())));
            } else if (Character.isDigit(current())) {
                if (skippinngWhitespace) {
                    Const c = parseConst(0);
                    parsingVarOrConst = false;
                    return new CheckedNegate(c);
                } else {
                    Const c = parseConst(1);
                    parsingVarOrConst = false;
                    return c;
                }
            } else if (take('-')) {
                return new CheckedNegate(parseUnaryMinus());
            }
            throw error("Invalid UnariMinus in area: " + errorBuffer);
        }

        private TripleExpression parseThirdOperation(char type) {
            parsingVarOrConst = true;
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
                    return new CheckedAdd(lastParsing, thirdOperation);
                }
            } else if (type == '-') {
                if (currentIsParsTern() || current('-')) {
                    while (parsingThirdOperation) {
                        thirdOperation = parseElement();
                    }
                    startingMinus = false;
                    return new CheckedSubtract(lastParsing, thirdOperation);
                }
            }
            throw error("Invalid arithmetic operation in area: " + errorBuffer);
        }

        private TripleExpression parseSecondOperation() {
            startingMinus = false;
            parsingVarOrConst = true;
            if (take('*')) {
                if (currentIsParsTern()) {
                    return new CheckedMultiply(parsingExpr, parseElement());
                } else if (take('-')) {
                    return new CheckedMultiply(parsingExpr, parseUnaryMinus());
                }
            } else if (take('/')) {
                if (currentIsParsTern()) {
                    return new CheckedDivide(parsingExpr, parseElement());
                } else if (take('-')) {
                    return new CheckedDivide(parsingExpr, parseUnaryMinus());
                }
            }
            throw error("Invalid arithmetic operation in area: " + errorBuffer);
        }

        private TripleExpression parseFourthOperation(int type) {
            parsingVarOrConst = true;
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
            throw error("Invalid arithmetic operation in area: " + errorBuffer);
        }
        private TripleExpression parseUnaryOperation(int type) {
            parsingVarOrConst = true;
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
                    throw error("Incorrectly bracketOperation in area: " + errorBuffer.toString());
                }
                bracket = parseElement();
            }
            if (parsingVarOrConst) {
                throw error("Incorrectly bracketOperation, missing arguments in area: " + errorBuffer);
            }
            parsingThirdOperation = bracketParsingThirdOperation;
            parsingFourthOperation = bracketParsingFourthOperation;
            startingMinus = false;
            return bracket;
        }
    }
}
