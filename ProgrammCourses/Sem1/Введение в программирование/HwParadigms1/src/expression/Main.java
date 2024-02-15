package expression;

import expression.parser.*;

public class Main {
    public static void main(String[] args) {
        String input = args[0];
        System.err.println(args[0]);
        //System.out.println(new Min(new Const(3), new Max(new Variable("x"), new Const(-5))).toString());
        System.out.println(Integer.toBinaryString(-132).length());
        //System.out.println(new ExpressionParser().parse(input).evaluate(-1083514580, 362659947, -1961030100));
        System.out.println(new ExpressionParser().parse(input).toString());
        //System.out.println((1158648961 * 1158648961 - -482464304) / 1972960255);
    }
}
