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

public class B {
    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String str = in.readLine();
        int len = str.length();
        Map<String, Integer> map = new TreeMap<>();
        if (len == 1) {
            System.out.println(str);
        } else {
            for (int i = 0; i < len; i++) {
                if (map.containsKey(str)) {
                    int c = map.get(str);
                    c++;
                    map.put(str, c);
                } else {
                    map.put(str, 1);
                }
                str = str.substring(len - 1, len) + str.substring(0, len - 1);
            }
            Set<Map.Entry<String, Integer>> strs = map.entrySet();
            StringBuilder sb = new StringBuilder();
            for(Map.Entry<String, Integer> s : strs) {
                for (int i = 0; i < s.getValue(); i++) {
                    sb.append(s.getKey().charAt(len - 1));
                }
            }
            System.out.print(sb.toString());
        }    
    }
}