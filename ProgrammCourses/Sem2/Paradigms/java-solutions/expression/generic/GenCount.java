package expression.generic;

public class GenCount<T extends GenericExprType<I>, I> extends GenUnaryOperation<T, I> {

    public GenCount(Exp<T, I> expression) {
        super(expression);
        typeOperation = "count";
    }

    @Override
    protected T toOperation(T x) {
        x.count();
        return x;
    }
}
