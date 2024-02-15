package expression.generic;

public class GenVariable<T extends GenericExprType<I>, I> implements Exp<T, I> {
    protected String var;
    private final T exprType;

    public GenVariable(String var, T exprType) {
        this.exprType = exprType;
        this.var = var;
    }

    @Override
    public T evaluateImpl(T c1, T c2, T c3) {
        switch (var) {
            case "x" -> exprType.constant(c1.val());
            case "y" -> exprType.constant(c2.val());
            case "z" -> exprType.constant(c3.val());
            default -> throw new IllegalArgumentException("Var must be x, y, z");
        }
        return exprType;
    }

    @Override
    public String toString() {
        return var;
    }
}
