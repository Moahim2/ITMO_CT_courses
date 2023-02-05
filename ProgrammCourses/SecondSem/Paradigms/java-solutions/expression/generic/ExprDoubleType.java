package expression.generic;

public class ExprDoubleType extends GenericExprType<Double> {
    public ExprDoubleType(Double val) {
        super(val);
    }

    @Override
    public <T extends GenericExprType<Double>> void add(T x) {
        val += x.val;
    }

    @Override
    public <T extends GenericExprType<Double>> void sub(T x) {
        val -= x.val;
    }

    @Override
    public <T extends GenericExprType<Double>> void mul(T x) {
        val *= x.val;
    }

    @Override
    public <T extends GenericExprType<Double>> void div(T x) {
        val /= x.val;
    }

    @Override
    protected Double val() {
        return val;
    }

    @Override
    public void neg() {
        val = -val;
    }

    @Override
    public String toString() {
        return Double.toString(val);
    }

    @Override
    public void constant(Object val) {
        this.val = (Double) val;
    }

    @Override
    public void addInt(int x) {
        val += x;
    }


    @Override
    public <T extends GenericExprType<Double>> void max(T a) {
        val = Math.max(val, a.val);
    }

    @Override
    public <T extends GenericExprType<Double>> void min(T a) {
        val = Math.min(val, a.val);
    }


    @Override
    public void count() {
        val = (double) Long.bitCount(Double.doubleToLongBits(val));
    }


    //Unused in HW5
    @Override
    public void l0() { //No
        throw new UnsupportedOperationException("Double hasn't got L0");
    }

    @Override
    public void t0() { //No
        throw new UnsupportedOperationException("Double hasn't got T0");
    }
}
