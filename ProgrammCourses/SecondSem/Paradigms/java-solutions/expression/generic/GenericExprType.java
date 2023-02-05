package expression.generic;


public abstract class GenericExprType <I> {
     protected I val;

     public GenericExprType(I val) {
          this.val = val;
     }

     protected abstract <T extends GenericExprType<I>> void add(T a);
     protected abstract <T extends GenericExprType<I>> void sub(T a);
     protected abstract <T extends GenericExprType<I>> void mul(T a);
     protected abstract <T extends GenericExprType<I>> void div(T a);
     protected abstract I val();
     protected abstract void constant(Object val);
     protected abstract void neg();
     protected abstract void addInt(int x);
     public abstract <T extends GenericExprType<I>> void max(T a);
     public abstract <T extends GenericExprType<I>> void min(T a);
     public abstract void count();

     //Unsupported operation + Unused in HW5
     public abstract void l0();
     public abstract void t0();
}
