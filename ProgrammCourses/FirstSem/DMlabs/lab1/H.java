import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.IOException;
public class H {
    static String func(int n) {
        String x = Integer.toString(n - 1);
        if (n == 1) {
            return "((A0|B0)|(A0|B0))";
        } else {
            return "((A" + x + "|B" + x + ")|(((A" + x + "|A" + x + ")|(B" + x + "|B" + x + "))|" + func(n - 1) + "))";    
        }
    }
    public static void main(String[] arg) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            int n = Integer.parseInt(in.readLine());
            if (n == 1) {
                System.out.print("((A0|B0)|(A0|B0))");
            } else {
                System.out.print(func(n));
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}