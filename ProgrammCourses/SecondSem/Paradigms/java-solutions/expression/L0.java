package expression;

import java.util.Objects;

public class L0 implements TripleExpression, Expression {
    private final TripleExpression expression;

    public L0(TripleExpression expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate(int c1, int c2, int c3) {
        String binaryNum = Integer.toBinaryString(expression.evaluate(c1, c2, c3));
        if (binaryNum.equals("0")) {
            return 32;
        }
        return 32 - binaryNum.length();
    }
    public int evaluate(int c1) {
        String binaryNum = Integer.toBinaryString(expression.evaluate(c1, c1, c1));
        if (binaryNum.equals("0")) {
            return 32;
        }
        return 32 - binaryNum.length();
    }

    @Override
    public String toString() {
        return "l0(" + expression.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof L0) {
            final L0 l0 = (L0) obj;
            return Objects.equals(expression, l0.expression);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(expression);
    }
}
