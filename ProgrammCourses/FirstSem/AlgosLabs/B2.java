import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class B2 {

    public static void main(String[] args) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String str1 = in.readLine(); // считывание
            String[] str2 = str1.split(" ");
            int n = Integer.parseInt(str2[0]);
            int k = Integer.parseInt(str2[1]);
            String str3 = in.readLine();
            String[] str4 = str3.split(" ");
            int[] tabl = new int[n];
            for (int i = 0; i < n; i++) {
                tabl[i] = Integer.parseInt(str4[i]); //
            }
            int l = -1;
            int r = (tabl[n - 1] - tabl[0]) + 1;
            while (r > l + 1) {
               int m = (l + r) / 2;
               int count = 1;
               int a = 0;
               for (int i = 1; i < n; i++) {
                   if (tabl[i] - tabl[a] > m) {
                       count++;
                       a = i;
                   }
               }
               if (count >= k) {
                   l = m;
               } else {
                   r = m;
               }
            }
            System.out.print(l + 1);
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}