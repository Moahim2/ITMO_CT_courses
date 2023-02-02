import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.HashSet;

public class C_Count_All_Words_In_DKA {

    static int n;
    static int m;
    static int k;
    static HashSet<Integer> terminals;
    static HashSet<Integer>[] ways;
    static HashSet<Integer>[] reverse_ways;
    static HashMap<Integer, Integer>[] reverse_ways_all;

    public static final int c = (10 * 10 * 10) * (10 * 10 * 10) * (10 * 10 * 10) + 7;


//
    static boolean[] good_nodes;
    public static void f_b_nImpl(int node) {
        good_nodes[node] = true;
        for (int i : reverse_ways[node]) {
            if (!good_nodes[i]) {
                f_b_nImpl(i);
            }
        }
    }
    public static void find_bad_nodes() {
        for (int it : terminals) {
            f_b_nImpl(it);
        }
    }
//

//
    static int[] last_nodes;
 //   static HashSet<Integer> l_nodes;
    static boolean hasCycle = false;
    public static void find_cycle(int node) {
        last_nodes[node] = 1;
        for (int i : ways[node]) {
            //System.out.println(i);
            if (last_nodes[i] == 1 && good_nodes[i]) {
                hasCycle = true;
                return;
            }
            if (last_nodes[i] == 0 && good_nodes[i]) {
                find_cycle(i);
            }
        }
        last_nodes[node] = 2;
    }
//


//
    public static int[] d_p;
    public static boolean[] was_nodes;
    public static int get_count_words(int node) {
        if (!good_nodes[1]) {
            return 0;
        }
        return get_count_wordsImpl(node);
    }
    public static int get_count_wordsImpl(int node) {
        if (was_nodes[node]) {
            return d_p[node];
        }
        was_nodes[node] = true;
        int ans = 0;
        for (var i : reverse_ways_all[node].entrySet()) {
            ans = (ans + get_count_wordsImpl(i.getKey()) * i.getValue()) % c;
        }
        d_p[node] = ans;
        return ans;
    }


    public static void main(String[] args) throws IOException {
        BufferedReader source = new BufferedReader(new InputStreamReader(new FileInputStream("problem3.in")));
        String[] nmk = source.readLine().split(" ");
        n = Integer.parseInt(nmk[0]);
        m = Integer.parseInt(nmk[1]);
        k = Integer.parseInt(nmk[2]);
        terminals = new HashSet<>();
        String[] terminalsInput = source.readLine().split(" ");
        for (int i = 0; i < k; ++i) {
            terminals.add(Integer.parseInt(terminalsInput[i]));
        }

        ways = new HashSet[n + 1];
        reverse_ways = new HashSet[n + 1];
        reverse_ways_all = new HashMap[n + 1];
        for (int i = 0; i < n + 1; ++i) {
            ways[i] = new HashSet<>();
            reverse_ways[i] = new HashSet<>();
            reverse_ways_all[i] = new HashMap<>();
        }


        for (int i = 0; i < m; ++i) {
            String[] tmp = source.readLine().split(" ");
            //char ch = tmp[2].charAt(0);
            int a = Integer.parseInt(tmp[0]);
            int b = Integer.parseInt(tmp[1]);
            ways[a].add(b);
            reverse_ways[b].add(a);
            if (!reverse_ways_all[b].containsKey(a)) {
                reverse_ways_all[b].put(a, 1);
            } else {
                reverse_ways_all[b].put(a, reverse_ways_all[b].get(a) + 1);
            }
        }
        good_nodes = new boolean[n + 1];
        for (int i = 0; i < n + 1; i++) {
            good_nodes[i] = false;
        }
        source.close();


        //1
        find_bad_nodes();
        //System.out.println(good_nodes);
            //1


        //2
                                    //l_nodes = new HashSet<>();
        last_nodes = new int[n + 1];
        for (int i = 0; i < n + 1; ++i) {
            last_nodes[i] = 0;
        }
        find_cycle(1);
        //System.out.println(hasCycle);
            //2
        ///////
        int ans = 0;
        if (!hasCycle) {
            was_nodes = new boolean[n + 1];
            d_p = new int[n + 1];
            d_p[1] = 1;
            was_nodes[1] = true;
            for (int i : terminals) {
                ans = (ans + get_count_words(i)) % c;
            }
        }
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("problem3.out"),
                StandardCharsets.UTF_8));

        //



        if (hasCycle) {
            out.write(Integer.toString(-1));
        } else {
            out.write(Integer.toString(ans));
        }
        out.close();
    }
}
