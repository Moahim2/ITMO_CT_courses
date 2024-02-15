package expression.generic;

public class GenConst<T extends GenericExprType<I>, I> implements Exp<T, I> {
    final T exprType;
    final I finValue;

    public GenConst(T exprType) {
        this.exprType = exprType;
        this.finValue = exprType.val;
    }

    @Override
    public T evaluateImpl(T c1, T c2, T c3) {
        exprType.constant(finValue);
        return exprType;
    }

    @Override
    public String toString() {
        return exprType.toString();
    }
}
