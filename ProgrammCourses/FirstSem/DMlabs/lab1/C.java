import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;
public class C {
    public static void main(String[] arg) {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(in.readLine());
        int l = n;
        ArrayList<String> k = new ArrayList<>();
        int last = 0;
        while (l > 0) {
            String str = in.readLine();
            if (str.length() >= 2 && last == 0) {
                l++;
                last = 1;
            } else {
                last = 0;
            }
            k.add(str);
        }
        String[][] args = new String[k.size()][1];
        for (int i = 0; i < k.size(); i++) {
            args[i][0] = k.get(i);
            if (k.get(i).length() >= 2) {
                
            }
        } 
    }
}