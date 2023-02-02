import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class A10 {
    public static StringBuilder prefix = new StringBuilder();
    public static int size = 0;
    public static int sum;
    public static int n;
    public static int last;
    public static int lastLast;
    
    public static void createVector() {
        if (size > 0) {
            if (sum == n) {
                prefix.delete(size - 1, size);
                System.out.println(prefix);
                return;
            }
            for (int i = last; i <= n - sum; i++) {
                int length;
                prefix.append(i).append('+');
                length = Integer.toString(i).length();
                size += length + 1;
                sum += i;
                lastLast = last;
                last = i;
                createVector();
                prefix.delete(size - 1 - length, size);
                size -= length + 1;
                sum -= i;
                last = lastLast;
            }
        } else {
            for (int i = 1; i < n + 1; i++) {
                int length;
                prefix.append(i).append('+');
                sum += i;
                lastLast = last;
                last = i;
                length = Integer.toString(i).length();
                size += length + 1;
                createVector();
                prefix.delete(size - length - 1, size);
                size -= length + 1;
                sum -= i;
                last = lastLast;
            }
        }
    }




    public static void main(String[] args) throws IOException {
        Scanner in = new Scanner(System.in);
        int r = in.nextInt();
        in.close();
        n = r;
        createVector();
    }
}
