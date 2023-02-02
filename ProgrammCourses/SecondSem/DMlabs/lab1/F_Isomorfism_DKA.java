import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.HashSet;


public class F_Isomorfism_DKA {
    static int n1;
    static int m1;
    static int k1;
    static int n2;
    static int m2;
    static int k2;

    static HashSet<Integer> terminals1;
    static HashSet<Integer> terminals2;
    static HashMap<Character, Integer>[] ways1;
    static HashMap<Character, Integer>[] ways2;


    static boolean[] arrPath;
    static int[] dfsPair;

    public static boolean dfs(Integer a1, Integer a2) {
        arrPath[a1] = true;

        if (terminals1.contains(a1) != terminals2.contains(a2)) {
            return false;
        }
        dfsPair[a1] = a2;

        if (ways1[a1].isEmpty()) {
            return ways2[a2].isEmpty();
        }
        if (ways2[a2].isEmpty()) {
            return ways1[a1].isEmpty();
        }

        for (var it : ways2[a2].entrySet()) {
            if (!ways1[a1].containsKey(it.getKey())) {
                return false;
            }
        }

        for (var it : ways1[a1].entrySet()) {
            if (!ways2[a2].containsKey(it.getKey())) {
                return false;
            }
            if (arrPath[it.getValue()]) {
                if (ways2[a2].get(it.getKey()) != dfsPair[it.getValue()]) {
                    return false;
                }
            } else {
                boolean t = dfs(it.getValue(), ways2[a2].get(it.getKey()));
                if (!t) {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean find_isomorphism() {
        if (k1 != k2 || n1 != n2 || m1 != m2) {
            return false;
        }
        arrPath = new boolean[n1 + 1];
        for (int i = 0; i < n1 + 1; ++i) {
            arrPath[i] = false;
        }
        dfsPair = new int[n1 + 1];
        return dfs(1, 1);
    }




    public static void main(String[] args) throws IOException {
        BufferedReader source = new BufferedReader(new InputStreamReader(new FileInputStream("isomorphism.in")));
        String[] nmk1 = source.readLine().split(" ");
        n1 = Integer.parseInt(nmk1[0]);
        m1 = Integer.parseInt(nmk1[1]);
        k1 = Integer.parseInt(nmk1[2]);

        terminals1 = new HashSet<>();
        String[] terminalsInput1 = source.readLine().split(" ");
        for (int i = 0; i < terminalsInput1.length; ++i) {
            terminals1.add(Integer.parseInt(terminalsInput1[i]));
        }

        ways1 = new HashMap[n1 + 1];
        for (int i = 0; i < n1 + 1; ++i) {
            ways1[i] = new HashMap<>();
        }
        for (int i = 0; i < m1; ++i) {
            String[] tmp = source.readLine().split(" ");
            char ch = tmp[2].charAt(0);
            int a = Integer.parseInt(tmp[0]);
            int b = Integer.parseInt(tmp[1]);
            ways1[a].put(ch, b);
        }


        String[] nmk2 = source.readLine().split(" ");
        n2 = Integer.parseInt(nmk2[0]);
        m2 = Integer.parseInt(nmk2[1]);
        k2 = Integer.parseInt(nmk2[2]);

        terminals2 = new HashSet<>();
        String[] terminalsInput2 = source.readLine().split(" ");
        for (int i = 0; i < terminalsInput2.length; ++i) {
            terminals2.add(Integer.parseInt(terminalsInput2[i]));
        }

        ways2 = new HashMap[n2 + 1];
        for (int i = 0; i < n2 + 1; ++i) {
            ways2[i] = new HashMap<>();
        }
        for (int i = 0; i < m2; ++i) {
            String[] tmp = source.readLine().split(" ");
            char ch = tmp[2].charAt(0);
            int a = Integer.parseInt(tmp[0]);
            int b = Integer.parseInt(tmp[1]);
            ways2[a].put(ch, b);
        }

        boolean ans = find_isomorphism();
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("isomorphism.out"),
                StandardCharsets.UTF_8));
        if (ans) {
            out.write("YES");
        } else {
            out.write("NO");
        }
        out.close();
    }
}
