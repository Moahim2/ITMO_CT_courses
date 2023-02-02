import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class A8 {
    public static void createVector(int n, int k, ArrayList<Integer> prefix) {
        if (prefix.size() == k) {
            for(int i = 0; i < k; i++) {
                System.out.print(prefix.get(i) + " ");
            }
            System.out.println();
            return;
        }
        int last;
        if (prefix.size() >= 1) {
            last = prefix.get(prefix.size() - 1);
            for (int i = 1; i < n + 1; i++) {
                if (i > last) {
                    prefix.add(i);
                    createVector(n, k, prefix);
                    prefix.remove(prefix.size() - 1);
                }
            }
        } else {
            for (int i = 1; i < n + 1; i++) {
                prefix.add(i);
                createVector(n, k, prefix);
                prefix.remove(prefix.size() - 1);
            }
        }
    }


    public static void main(String[] args) throws IOException {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int k = in.nextInt();
        createVector(n, k,new ArrayList<>());
    }
}
