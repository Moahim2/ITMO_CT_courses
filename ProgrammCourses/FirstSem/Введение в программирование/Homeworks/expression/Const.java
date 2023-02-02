package expression;

import java.util.Objects;

public class Const implements TripleExpression, Expression {
    protected int a;

    public Const(int a) {
        this.a = a;
    }

    @Override
    public int evaluate(int c1, int c2, int c3) {
        return a;
    }

    public int evaluate(int c1) {
        return a;
    }

    @Override
    public String toString() {
        return Integer.toString(a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Const) {
            final Const cons = (Const) obj;
            return Objects.equals(a, cons.a);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(a);
    }
}
