package expression;
import java.lang.Math.*;

public class Min extends Operation{

    public Min(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "min";
    }

    @Override
    protected int toOperation(int x, int y) {
        return Math.min(x, y);
    }

}
