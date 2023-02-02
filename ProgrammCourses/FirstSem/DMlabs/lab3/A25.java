import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;


public class A25 {

    public static void main(String[] args) throws IOException {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int k = in.nextInt();
        int[] vector = new int[k];
        for (int i = 0; i < k; i++) {
            vector[i] = in.nextInt();
        }
        in.close();
        int max = n;
        int j = k - 1;
        int count = 0;
        while (j >= 0 && vector[j] - max == 0) {
            max--;
            j--;
            count++;
        }
        if (j == -1) {
            System.out.println(-1);
        } else {
            vector[j] = vector[j] + 1;
            for (int i = j; i < k - 1; i++) {
                if (vector[i + 1] - vector[i] > 1) {
                    vector[i + 1] = vector[i] + 1;
                }
            }
            for (int i = 0; i < k; i++) {
                System.out.print(vector[i] + " ");
            }
        }
    }    
}
