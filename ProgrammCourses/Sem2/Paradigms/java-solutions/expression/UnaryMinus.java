package expression;

import java.util.Objects;

public class UnaryMinus implements TripleExpression, Expression {
    private final TripleExpression expression;

    public UnaryMinus(TripleExpression expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate(int c1, int c2, int c3) {
        return -expression.evaluate(c1, c2, c3);
    }
    public int evaluate(int c1) {
        return -expression.evaluate(c1, c1, c1);
    }

    @Override
    public String toString() {
        return "-(" + expression.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof UnaryMinus) {
            final UnaryMinus unaryMinus = (UnaryMinus) obj;
            return Objects.equals(expression, unaryMinus.expression);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(expression);
    }
}
