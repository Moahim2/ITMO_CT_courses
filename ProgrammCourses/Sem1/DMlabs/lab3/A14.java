import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;


public class A14 {
    public static int n;
    public static long phactor = 1;
    public static int[] permutation;
    public static long number = 0;
    public static int iterator = 0;
    public static HashSet<Integer> elements = new HashSet<Integer>();
    public static int length = 0;

    public static void createNumber() {
        for (int i = 1; i <= permutation.length; i++) {
            if (i < permutation[iterator]) {
                if (!elements.contains(i)) {
                    number += phactor;
                }
            } else if (i == permutation[iterator]) {
                if (length != permutation.length) {
                    elements.add(i);
                    iterator++;
                    length++;
                    if (iterator >= permutation.length - 1) {
                        phactor = 1;
                        return;
                    } else {
                        phactor = phactor / --n;
                    }
                    createNumber();
                    break;
                } else {
                    return;
                }
            }
        }
    }
    public static void main(String[] args) throws IOException {
        Scanner in = new Scanner(System.in);
        n = in.nextInt();
        permutation = new int[n];
        for (int i = 0; i < n; i++) {
            permutation[i] = in.nextInt();
        }
        in.close();
        for (int i = 1; i <= n - 1; i++) {
            phactor *= i;
        }
        createNumber();
        System.out.println(number);
    }
}
