import java.io.*;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class B_StringInNKA {
    public static HashMap<Character, HashSet<Integer>>[] ways;
    public static Set<Integer> terminals = new HashSet<>();
    public static String inputString;

//    public static boolean fCheck(int curSym, int current) {
//        if (curSym == inputString.length()) {
//            return terminals.contains(current);
//        }
//        char ch = inputString.charAt(curSym);
//        if (!ways[current].containsKey(ch)) {
//            return false;
//        }
//        boolean tmp = false;
//        for (Integer it : ways[current].get(ch)) {
//            tmp = fCheck(curSym + 1, it);
//            if (tmp) {
//                return true;
//            }
//        }
//        return false;
//    }
    public static void main(String[] args) throws IOException {
        BufferedReader source = new BufferedReader(new InputStreamReader(new FileInputStream("problem2.in")));
        inputString = source.readLine();
        String[] nmk = source.readLine().split(" ");
        int n = Integer.parseInt(nmk[0]);
        int m = Integer.parseInt(nmk[1]);
        int k = Integer.parseInt(nmk[2]);
        String[] terminalsInput = source.readLine().split(" ");
        for (int i = 0; i < terminalsInput.length; ++i) {
            terminals.add(Integer.parseInt(terminalsInput[i]));
        }
        ways = new HashMap[n + 1];
        for (int i = 0; i < n + 1; ++i) {
            ways[i] = new HashMap<>();
        }
        for (int i = 0; i < m; ++i) {
            String[] tmp = source.readLine().split(" ");
            char ch = tmp[2].charAt(0);
            int a = Integer.parseInt(tmp[0]);
            int b = Integer.parseInt(tmp[1]);
            if (!ways[a].containsKey(ch)) {
                ways[a].put(ch, new HashSet<>());
            }
            ways[a].get(ch).add(b);
        }
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("problem2.out")));
        if (k == 0 || n == 0) {
            out.write("Rejects");
            out.close();
            return;
        }
        boolean ans = false;
        HashSet<Integer>[] arr = new HashSet[inputString.length() + 1];
        for (int i = 0; i < inputString.length() + 1; ++i) {
            arr[i] = new HashSet<>();
        }
        for (int i = 1; i < n + 1; ++i) {
            arr[0].add(i);
        }

        for (int i = 1; i < arr.length; ++i) {
            for (Integer q : arr[i - 1]) {
                if (ways[q] != null) {
                    if (ways[q].containsKey(inputString.charAt(i - 1))) {
                        for (Integer g : ways[q].get(inputString.charAt(i - 1))) {
                            arr[i].add(g);
                        }
                    }
                }
            }
        }

        for (Integer q : arr[inputString.length()]) {
            if (terminals.contains(q)) {
                ans = true;
                break;
            }
        }

        if (ans) {
            out.write("Accepts");
        } else {
            out.write("Rejects");
        }
        out.close();
    }
}
