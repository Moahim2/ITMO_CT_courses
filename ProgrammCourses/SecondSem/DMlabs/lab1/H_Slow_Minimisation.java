import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class H_Slow_Minimisation {

    static boolean[] reach;

    static class Pair {
        int first;
        int second;
        Pair(int term, int notTerm) {
            this.first = term;
            this.second = notTerm;
        }
    }

    static int n;
    static int m;
    static int k;
    static HashSet<Integer> terminals;
    static HashMap<Character, Integer>[] ways;
    static HashMap<Character, HashSet<Integer>>[] r_ways;
    static HashSet<Character> alph;
    static HashSet<Integer>[] reverse_ways;
//    static HashMap<Integer, Integer>[] reverse_ways_all;
    static boolean[] good_nodes;

    public static void f_b_nImpl(int node) {
        //System.out.println(node);
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
        for (int i = 1; i < n + 1; ++i) {
            if (!good_nodes[i]) {
                reach[i] = false;
            }
        }
        //System.out.println(Arrays.toString(reach));
    }

    static void getReach(int node) {
        //System.out.println(ways[1]);
        reach[node] = true;
        for (var i : ways[node].entrySet()) {
            if (!reach[i.getValue()]) {
                getReach(i.getValue());
            }
        }
    }

    public static void main(String[] args) throws IOException {

        BufferedReader source = new BufferedReader(new InputStreamReader(new FileInputStream("minimization.in")));
        String[] nmk = source.readLine().split(" ");
        n = Integer.parseInt(nmk[0]);
        m = Integer.parseInt(nmk[1]);
        k = Integer.parseInt(nmk[2]);
        terminals = new HashSet<>();
        String[] terminalsInput = source.readLine().split(" ");
        for (int i = 0; i < k; ++i) {
            terminals.add(Integer.parseInt(terminalsInput[i]));
        }

        ways = new HashMap[n + 1];
        r_ways = new HashMap[n + 1];
        reverse_ways = new HashSet[n + 1];
//        reverse_ways_all = new HashMap[n + 1];
        for (int i = 0; i < n + 1; ++i) {
            ways[i] = new HashMap<>();
            r_ways[i] = new HashMap<>();
            reverse_ways[i] = new HashSet<>();
//            reverse_ways_all[i] = new HashMap<>();
        }

        alph = new HashSet<>();
        for (int i = 0; i < m; ++i) {
            String[] tmp = source.readLine().split(" ");
            char ch = tmp[2].charAt(0);
            int a = Integer.parseInt(tmp[0]);
            int b = Integer.parseInt(tmp[1]);
            if (!r_ways[b].containsKey(ch)) {
                r_ways[b].put(ch, new HashSet<>());
            }
            r_ways[b].get(ch).add(a);
            ways[a].put(ch, b);
            reverse_ways[b].add(a);
//            if (!reverse_ways_all[b].containsKey(a)) {
//                reverse_ways_all[b].put(a, 1);
//            } else {
//                reverse_ways_all[b].put(a, reverse_ways_all[b].get(a) + 1);
//            }1
            alph.add(ch);
        }
        //System.out.println(ways[1]);
        reach = new boolean[n + 1];
        good_nodes = new boolean[n + 1];
        source.close();
        getReach(1);
        find_bad_nodes();
//
//        for (int i = 1; i < n + 1; i++) {
//            for (var ch : alph) {
//                if (!ways[i].containsKey(ch)) {
//                    ways[i].put(ch, 0);
//                }
//            }
//        }
        for (var ch : alph) {
            ways[0].put(ch, 0);
            r_ways[0].put(ch, new HashSet<>());
            var tmp = r_ways[0].get(ch);
            tmp.add(0);
            for (int i = 1; i < n + 1; i++) {
                if (!ways[i].containsKey(ch)) {
                   // System.out.println(ch);
                    tmp.add(i);
                }
            }
        }
        //
        Stack<Pair> stack = new Stack<>();
        boolean[][] eq = new boolean[n + 1][n + 1];
        for (int i : terminals) {
                for (int j = 0; j < n + 1; ++j) {
                    if (!terminals.contains(j)) {
                        stack.push(new Pair(i, j));
                        stack.push(new Pair(j, i));
                        eq[i][j] = true;
                        eq[j][i] = true;
                        //System.out.println(i + " " + j);
                    }
                }
        }
        /////
//        for (int i = 1; i < n + 1; i++) {
//            if (reach[i]) {
//                stack.push(new Pair(i, i));
//            }
//        }
        //System.out.println(Arrays.toString(reach));
        /////

        while (!stack.isEmpty()) {
            Pair p = stack.pop();
            int p1 = p.first;
            int p2 = p.second;
            for (Character ch : alph) {
                if (r_ways[p1].containsKey(ch) && r_ways[p2].containsKey(ch)) {
                    for (int i : r_ways[p1].get(ch)) {
                        for (int j : r_ways[p2].get(ch)) {
                            if (!eq[i][j]) {
                                stack.add(new Pair(i, j));
                                eq[i][j] = true;
                            }
                        }
                    }
                }
            }
        }
//        for (int i = 1; i < eq.length; i++) {
//            for (int j = 1; j < eq.length; j++) {
//                String s = "f";
//                if (eq[i][j]) {
//                    s = "t";
//                }
//                System.out.print(s + "  ");
//            }
//            System.out.println();
//        }
        Set<HashSet<Integer>> eqs = new HashSet<>();
        ArrayList<TreeMap<Character, Integer>> new_ways = new ArrayList<>();
        new_ways.add(new TreeMap<>());
        int[] w_eqs = new int[n + 1];
        //eqs.add(new HashSet<>());
        for (int i = 1; i < n + 1; ++i) {
            HashSet<Integer> tmp = new HashSet<>();
            if (reach[i]) {
                tmp.add(i);
                for (int j = 1; j < n + 1; j++) {
                    if (!eq[i][j] && reach[j]) {
                        tmp.add(j);
                    }
                }
                boolean flag = false;
                for (var s : eqs) {
                    if (s.equals(tmp)) {
                        flag = true;
                        break;
                    }
                }
                if (!flag) {
                    eqs.add(tmp);
                    new_ways.add(new TreeMap<>());
                    for (int x : tmp) {
                        w_eqs[x] = eqs.size();
                    }
                }
            }
        }
        //System.out.println(eqs);
        int new_m = 0;
        TreeSet<Integer> new_terminals = new TreeSet<>();
        for (var e : eqs) {
            for (int i : e) {
                if (terminals.contains(i)) {
                    new_terminals.add(w_eqs[i]);
                }
                HashMap<Character, Integer> t = ways[i];
                for (var j : t.entrySet()) {
                    //справа проверка на плохие состояния
                    if (!new_ways.get(w_eqs[i]).containsKey(j.getKey()) &&  w_eqs[j.getValue()] != 0) {
                        new_m++;
                        new_ways.get(w_eqs[i]).put(j.getKey(), w_eqs[j.getValue()]);
                    }
                }
            }
        }
//        for (int i = 1; i < new_ways.size(); i++) {
//            System.out.println(new_ways.get(i));
//        }







        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("minimization.out"),
                StandardCharsets.UTF_8));

        if (new_terminals.size() == 0) {
            out.write(Integer.toString(0));
            out.write(" ");
            out.write(Integer.toString(0));
            out.write(" ");
            out.write(Integer.toString(0));
            out.newLine();

            //
            out.newLine();
            out.close();
            return;
        }
        out.write(Integer.toString(eqs.size()));
        out.write(" ");
        out.write(Integer.toString(new_m));
        out.write(" ");
        out.write(Integer.toString(new_terminals.size()));
        out.newLine();
        int x = 0;
        for (int i : new_terminals) {
            x++;
            out.write(Integer.toString(i));
            if (x != new_terminals.size()) {
                out.write(" ");
            }
        }
        out.newLine();
        int cou = 0;
        for (int i = 1; i < new_ways.size(); i++) {
            var t = new_ways.get(i);

            for (var j : t.entrySet()) {
                out.write(Integer.toString(i));
                out.write(" ");
                out.write(Integer.toString(j.getValue()));
                out.write(" ");
                out.write(j.getKey());

//                if (cou != new_m - 1) {
//                    out.newLine();
//                }
//                cou++;
                out.newLine();
            }
        }
        out.close();
    }
}
