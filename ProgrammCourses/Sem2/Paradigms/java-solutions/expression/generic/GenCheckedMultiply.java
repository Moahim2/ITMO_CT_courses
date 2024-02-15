package expression.generic;

public class GenCheckedMultiply<T extends GenericExprType<I>, I> extends GenBinaryOperation<T, I> {
    public GenCheckedMultiply(Exp<T, I> a, Exp<T, I> b) {
        super(a, b);
        typeOperation = "*";
    }

    @Override
    protected T toOperation(T x, T y) {
        x.mul(y);
        return x;
    }
}
