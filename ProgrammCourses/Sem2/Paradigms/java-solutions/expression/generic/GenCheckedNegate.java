package expression.generic;

public class GenCheckedNegate<T extends GenericExprType<I>, I> extends GenUnaryOperation<T, I> {

    public GenCheckedNegate(Exp<T, I> expression) {
        super(expression);
        typeOperation = "-";
    }

    @Override
    protected T toOperation(T x) {
        x.neg();
        return x;
    }
}
