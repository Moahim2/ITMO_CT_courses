import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class A7 {
    public static void createVector(int n, ArrayList<Integer> prefix) {
        if (prefix.size() == n) {
            for(int i = 0; i < n; i++) {
                System.out.print(prefix.get(i) + " ");
            }
            System.out.println();
            return;
        }
        for (int i = 1; i < n + 1; i++) {
            if (!(prefix.contains(i))) {
                prefix.add(i);
                createVector(n, prefix);
                prefix.remove(prefix.size() - 1);
            }
        }
    }

    public static void main(String[] args) throws IOException {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        createVector(n, new ArrayList<>());
    }
}    
