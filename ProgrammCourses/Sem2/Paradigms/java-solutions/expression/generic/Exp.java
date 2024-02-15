package expression.generic;

public interface Exp<T extends GenericExprType<I>, I> {
    T evaluateImpl(T a, T b, T c);
    default Object evaluate(T a, T b, T c) {
        return evaluateImpl(a, b, c).val();
    }
}