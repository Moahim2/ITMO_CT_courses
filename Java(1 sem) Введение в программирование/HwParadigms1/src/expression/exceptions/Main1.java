package expression.exceptions;

public class Main1 {
    public static void main(String[] args) {
        String input = args[0];
        //System.out.println(Integer.toBinaryString(-132).length());
        System.out.println(new ExpressionParser().parse(input).toString());
        //System.out.println(new ExpressionParser().parse(input).evaluate(0,0,0));
    }
}
