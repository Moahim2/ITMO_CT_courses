import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.lang.Object.*;
public class G_EquivalenceDKA {
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

     static class Pair {
        int q1;
        int q2;
        Pair(int q1, int q2) {
            this.q1 = q1;
            this.q2 = q2;
        }
    }


    public static boolean find_equivalence() {

        Stack<Pair> S = new Stack<>();
        S.add(new Pair(1, 1));
        boolean[][] path = new boolean[n1 + 1][n2 + 1];
        for (int i = 0; i < n1 + 1; i++) {
            for (int j = 0; j < n2 + 1; j++) {
                path[i][j] = false;
            }
        }

        while (!S.isEmpty()) {
            Pair p = S.pop();
            int a1 = p.q1;
            int a2 = p.q2;
            path[a1][a2] = true;
            if (terminals1.contains(a1) != terminals2.contains(a2)) {
                return false;
            }

            for (var q1 : ways1[a1].entrySet()) {
                int V;
                V = ways2[a2].getOrDefault(q1.getKey(), 0);
                if (!path[q1.getValue()][V]) {
                    S.push(new Pair(q1.getValue(), V));
                }
            }

            for (var q2 : ways2[a2].entrySet()) {
                int V;
                V = ways1[a1].getOrDefault(q2.getKey(), 0);
                if (!path[V][q2.getValue()]) {
                    S.push(new Pair(V, q2.getValue()));
                }
            }
        }
        return true;
    }


    public static void main(String[] args) throws IOException {
        BufferedReader source = new BufferedReader(new InputStreamReader(new FileInputStream("equivalence.in")));
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
        boolean ans = find_equivalence();
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("equivalence.out"),
                StandardCharsets.UTF_8));
        if (ans) {
            out.write("YES");
        } else {
            out.write("NO");
        }
        out.close();
    }
}
