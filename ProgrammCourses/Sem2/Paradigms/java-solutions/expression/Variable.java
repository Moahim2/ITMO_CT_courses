package expression;

import java.util.Objects;

public class Variable implements TripleExpression, Expression {
    protected String var;

    public Variable(String var) {
        this.var = var;
    }

    @Override
    public int evaluate(int c1, int c2, int c3) {
        if (var.equals("x")) {
            return c1;
        } else if (var.equals("y")) {
            return c2;
        } else {
            return c3;
        }
    }

    public int evaluate(int c1) {
        return c1;
    }

    @Override
    public String toString() {
        return var;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Variable) {
            Variable variable = (Variable) obj;
            return Objects.equals(var, variable.var);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(var);
    }
}
