package expression.generic;

import java.math.BigInteger;
import java.util.Objects;

public class ExprBigIntegerType extends GenericExprType<BigInteger>{

    public ExprBigIntegerType(BigInteger val) {
        super(val);
    }

    @Override
    public <T extends GenericExprType<BigInteger>> void add(T x) {
        val = val.add(x.val);
    }

    @Override
    public <T extends GenericExprType<BigInteger>> void sub(T x) {
        val = val.subtract(x.val);
    }

    @Override
    public <T extends GenericExprType<BigInteger>> void mul(T x) {
        val = val.multiply(x.val);
    }

    @Override
    public <T extends GenericExprType<BigInteger>> void div(T x) {
        if (Objects.equals(x.val, new BigInteger("0"))) {
            throw new ArithmeticException("Divide by zero");
        }
        val = val.divide(x.val);
    }

    @Override
    protected BigInteger val() {
        return val;
    }


    @Override
    public void neg() {
        val = val.negate();
    }


    @Override
    public String toString() {
        return val.toString();
    }

    @Override
    public void constant(Object val) {
        this.val = (BigInteger) val;
    }

    @Override
    public void addInt(int x) {
        val = val.add(new BigInteger(Integer.toString(x)));
    }
    //Unsapported

    @Override
    public <T extends GenericExprType<BigInteger>> void max(T a) {
        val = val.max(a.val);
    }

    @Override
    public <T extends GenericExprType<BigInteger>> void min(T a) {
        val = val.min(a.val);
    }

    @Override
    public void count() {
        val = new BigInteger(Integer.toString(val.bitCount()));
    }


    //Unused in HW5
    @Override
    public void l0() { //No
        throw new UnsupportedOperationException("BigInteger hasn't got L0");
    }

    @Override
    public void t0() { //No
        throw new UnsupportedOperationException("BigInteger hasn't got T0");
    }
}
