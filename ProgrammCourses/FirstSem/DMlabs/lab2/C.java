import java.io.IOException;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException; 
import java.util.Set;
import java.util.HashSet;
import java.util.*;

public class C {
    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String str = in.readLine();
        int len = str.length();
        String[] chars = new String[len];
        for (int i = 0; i < len; i++) {
            chars[i] = Character.toString(str.charAt(i));
        }
        if (len == 1) {
            System.out.println(str);
        } else {
            String[] arr = new String[len];
            for (int i = 0; i < len; i++) {
                arr[i] = chars[i];
            }
            Arrays.sort(arr);
            for (int i = 0; i < len - 1; i++) {
                for (int j = 0; j < len; j++) {
                    arr[j] = chars[j] + arr[j];
                }
                Arrays.sort(arr);
            }
            System.out.println(arr[0]);
        }
    }
}