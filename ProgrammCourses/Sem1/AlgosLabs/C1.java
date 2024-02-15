import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;
public class C1 {
    static int[] baseNamberSort(int[] arr, int m) {
        int[] counter = new int[m];
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] < m) {
                counter[arr[i]]++;
            }
        }
        return counter;
    }
    public static void main(String[] args) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            int n = Integer.parseInt(in.readLine());
            String arg = in.readLine();
            String[] spl = new String [n];
            spl = arg.split(" ");
            int[] arr = new int [n];
            for (int i = 0; i < n; i++) {
                arr[i] = Integer.parseInt(spl[i]);
            }
            int[] counter = baseNamberSort(arr, n);
            int mex = -1;
            for (int i = 0; i < n; i++) {
                if (counter[i] == 0) {
                    mex = i;
                    break;
                }
            }
            if (mex != -1) {
                System.out.print(mex);
            } else {
                System.out.print(n);
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}