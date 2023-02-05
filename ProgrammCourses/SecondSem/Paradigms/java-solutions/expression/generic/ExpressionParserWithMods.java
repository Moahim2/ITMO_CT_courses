package expression.generic;

//import java.math.BigInteger;
//import java.util.function.BiFunction;
import java.util.function.Function;

public final class ExpressionParserWithMods<T extends GenericExprType<I>, I> {

    public Exp<T, I> parse(String expression, Function<String, T> f) throws ParsingException {
        //System.err.println(expression);
        return parse(new StringCharSource(expression), f);
    }

    public Exp<T, I> parse(final CharSourse source, Function<String, T> f) throws ParsingException {
        return new ExpressionPars<T, I>(source, f).parse();
    }

    private static class ExpressionPars<T extends GenericExprType<I>, I> extends BaseParser {
        private Exp<T, I> parsingExpr;
        private boolean startingMinus = true;
        private boolean parsingVarOrConst = true;
        private boolean parsingThirdOperation = false;
        private boolean parsingFourthOperation = false;

        private final Function<String, T> generateExprType;

        public ExpressionPars(CharSourse source, Function<String, T> f) {
            super(source);
            this.generateExprType = f;
        }



        public Exp<T, I> parse() throws ParsingException {
            try {
                Exp<T, I> result = parseElement();
                while (!end()) {
                    result = parseElement();
                }
                return result;
            } catch (ParsingException e) {
                throw new ParsingException(e.getMessage());
            }
        }

        private Exp<T, I> parseElement() throws ParsingException {
            skipWhitespace();
            final Exp<T, I> result = parseValue();
            skipWhitespace();
            return result;
        }

        private Exp<T, I> parseValue() throws ParsingException {

            if (take('l')) { //Unsupported and unused in 5 HW
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
            //

            if (take('c')) {
                expect('o');
                expect('u');
                expect('n');
                expect('t');
                if (!Character.isWhitespace(current()) && !current('(')) {
                    throw error("Parsing error, missing whitespace: " + errorBuffer);
                }
                parsingExpr = parseUnaryOperation(2);
                return parsingExpr;
            }
            if (between()) {
                parsingExpr = parseConst(0);
                return parsingExpr;
            }
            if (current('x') || current('y') || current('z')) {
                parsingExpr = parseVariable();
                return parsingExpr;
            }
            if (current('-')) {
                skipWhitespace();
                if (startingMinus) {
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
                            && !current('t') && !current('l') && !current('c')
                            && !current('x') &&
                            !current('y') && !current('z')) {
                        throw error("Parsing error, missing whitespace: " + errorBuffer);
                    }
                    parsingExpr = parseFourthOperation(0);
                    return parsingExpr;
                } else if (take('a')) {
                    expect('x');
                    if (!Character.isWhitespace(current()) && !current('(') && !current('-')
                            && !current('t') && !current('l') && !current('c')
                            && !current('x') && !current('y') && !current('z')) {
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
                    current('l') || current('c')|| current('t');
        }

        private GenConst<T, I> parseConst(int minus) throws ParsingException {
            if (!parsingVarOrConst) {
                throw error("Parsing error, missing sign between arguments: " + errorBuffer);
            }
            startingMinus = false;
            final StringBuilder number = new StringBuilder();
            if (take('-')) {
                number.append('-');
            }
            while (between()) {
                number.append(take());
            }
            try {
                parsingVarOrConst = false;
                if (minus == 0) {
                    return new GenConst<>(generateExprType.apply(number.toString()));
                } else {
                    return new GenConst<>(generateExprType.apply("-" + number));
                }
            } catch (NumberFormatException e) {
                throw error("Invalid number: " + number + ": " + e.getMessage() + " in area " + errorBuffer);
            }
        }

        private GenVariable<T, I> parseVariable() throws ParsingException {
            if (!parsingVarOrConst) {
                throw error("Parsing error, missing sign between arguments: " + errorBuffer);
            }
            startingMinus = false;
            final String variable = Character.toString(take());
            if (current('+') || current('-') || current('*') || current('/') ||
                    current('(') || current(')') ||Character.isWhitespace(current()) ||
                    current('\0') || current('m')) {
                parsingVarOrConst = false;
                return new GenVariable<>(variable, generateExprType.apply("0"));
            } else {
                throw error("Invalid Variable in area: " + errorBuffer);
            }
        }

        private Exp<T, I> parseUnaryMinus() throws ParsingException {
            parsingVarOrConst = true;
            startingMinus = false;
            boolean skippingWhitespace = false;

            if (Character.isWhitespace(current())) {
                skipWhitespace();
                skippingWhitespace = true;
            }

            if (current('(') || current('l') || current('t') || current('c')) {
                return new GenCheckedNegate<>(parseElement());
            } else if (current('x') || current('y') || current('z')) {
                parsingVarOrConst = false;
                return new GenCheckedNegate<>(new GenVariable<>(Character.toString(take()), generateExprType.apply("0")));
            } else if (Character.isDigit(current())) {
                GenConst<T, I> c;
                if (skippingWhitespace) {
                    c = parseConst(0);
                    parsingVarOrConst = false;
                    return new GenCheckedNegate<>(c);
                } else {
                    c = parseConst(1);
                    parsingVarOrConst = false;
                    return c;
                }
            } else if (take('-')) {
                return new GenCheckedNegate<>(parseUnaryMinus());
            }
            throw error("Invalid UnaryMinus in area: " + errorBuffer);
        }

        private Exp<T, I> parseThirdOperation(char type) throws ParsingException {
            parsingVarOrConst = true;
            parsingThirdOperation = true;
            startingMinus = true;
            Exp<T, I> thirdOperation = null;
            Exp<T, I> lastParsing = parsingExpr;

            if (type == '+') {
                if (currentIsParsTern() || current('-')) {
                    while (parsingThirdOperation) {
                        thirdOperation = parseElement();
                    }
                    startingMinus = false;
                    return new GenCheckedAdd<>(lastParsing, thirdOperation);
                }
            } else if (type == '-') {
                if (currentIsParsTern() || current('-')) {
                    while (parsingThirdOperation) {
                        thirdOperation = parseElement();
                    }
                    startingMinus = false;
                    return new GenCheckedSubtract<>(lastParsing, thirdOperation);
                }
            }
            throw error("Invalid arithmetic operation in area: " + errorBuffer);
        }

        private Exp<T, I> parseSecondOperation() throws ParsingException {
            startingMinus = false;
            parsingVarOrConst = true;
            if (take('*')) {
                if (currentIsParsTern()) {
                    return new GenCheckedMultiply<>(parsingExpr, parseElement());
                } else if (take('-')) {
                    return new GenCheckedMultiply<>(parsingExpr, parseUnaryMinus());
                }
            } else if (take('/')) {
                if (currentIsParsTern()) {
                    return new GenCheckedDivide<>(parsingExpr, parseElement());
                } else if (take('-')) {
                    return new GenCheckedDivide<>(parsingExpr, parseUnaryMinus());
                }
            }
            throw error("Invalid arithmetic operation in area: " + errorBuffer);
        }

        private Exp<T, I> parseFourthOperation(int type) throws ParsingException {
            parsingVarOrConst = true;
            parsingFourthOperation = true;
            startingMinus = true;
            Exp<T, I> fourthOperation = null;
            Exp<T, I> lastParsing = parsingExpr;

            if (type == 0) {
                if (currentIsParsTern() || current('-')) {
                    while (parsingFourthOperation) {
                        fourthOperation = parseElement();
                    }
                    startingMinus = false;
                    return new GenMin<>(lastParsing, fourthOperation);
                }
            } else if (type == 1) {
                if (currentIsParsTern() || current('-')) {
                    while (parsingFourthOperation) {
                        fourthOperation = parseElement();
                    }
                    startingMinus = false;
                    return new GenMax<>(lastParsing, fourthOperation);
                }
            }
            throw error("Invalid arithmetic operation in area: " + errorBuffer);
        }
        private Exp<T, I> parseUnaryOperation(int type) throws ParsingException {
            parsingVarOrConst = true;
            startingMinus = true;
            skipWhitespace();
            if (type == 0) {
                return new GenL0<>(parseElement());
            } else if (type == 1) {
                return new GenT0<>(parseElement());
            } else {
                return new GenCount<>(parseElement());
            }
        }

        private Exp<T, I> parseBracket() throws ParsingException {
            Exp<T, I> bracket = null;

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
