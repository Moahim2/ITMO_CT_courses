package expression.generic;

public class GenCheckedDivide<T extends GenericExprType<I>, I> extends GenBinaryOperation<T, I> {
    public GenCheckedDivide(Exp<T, I> a, Exp<T, I> b) {
        super(a, b);
        typeOperation = "/";
    }

    @Override
    protected T toOperation(T x, T y) {
        x.div(y);
        return x;
    }
}
