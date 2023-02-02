import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class I_Fast_Minimisation {

    static boolean[] reach;

    static class Pair_Node {
        int first;
        int second;
        Pair_Node(int term, int notTerm) {
            this.first = term;
            this.second = notTerm;
        }
    }

    static class Pair_Ch {
        int cl;
        char ch;
        Pair_Ch(int term, char notTerm) {
            this.cl = term;
            this.ch = notTerm;
        }
    }


    static int n;
    static int m;
    static int k;
    static HashSet<Integer> terminals;
    static HashMap<Character, Integer>[] ways;
    static TreeMap<Character, HashSet<Integer>>[] r_ways;
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
        BufferedReader source = new BufferedReader(new InputStreamReader(new FileInputStream("fastminimization.in")));
        String[] nmk = source.readLine().split(" ");
        n = Integer.parseInt(nmk[0]);
        m = Integer.parseInt(nmk[1]);
        k = Integer.parseInt(nmk[2]);
        terminals = new HashSet<>();
        HashSet<Integer> terminalscopy = new HashSet<>();
        String[] terminalsInput = source.readLine().split(" ");
        for (int i = 0; i < k; ++i) {
            terminals.add(Integer.parseInt(terminalsInput[i]));
            terminalscopy.add(Integer.parseInt(terminalsInput[i]));
        }

        ways = new HashMap [n + 1];
        r_ways = new TreeMap[n + 1];
        reverse_ways = new HashSet[n + 1];
//        reverse_ways_all = new HashMap[n + 1];
        for (int i = 0; i < n + 1; ++i) {
            ways[i] = new HashMap<>();
            r_ways[i] = new TreeMap<>();
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

       ArrayList<HashSet<Integer>> P = new ArrayList<>();
       Stack<Pair_Ch> Q = new Stack<>();

       //System.out.println(terminals);
       P.add(terminalscopy);
       HashSet<Integer> t1111 = new HashSet<>();
       for (int i = 0; i < n + 1; i++) {
           if (!terminals.contains(i)) {
               t1111.add(i);
           }
       }
       P.add(t1111);

       for (Character ch : alph) {
           Q.push(new Pair_Ch(0, ch));
           Q.push(new Pair_Ch(1, ch));
       }

       int[] Clss = new int[n + 1];
       for (int i = 0; i < n + 1; i++) {
           if (terminals.contains(i)) {
               Clss[i] = 0;
           } else {
               Clss[i] = 1;
           }
       }

       ////////////////////////////
       while (!Q.isEmpty()) {
           Pair_Ch pair_ch = Q.pop();
           int cl = pair_ch.cl;
           char ch = pair_ch.ch;
           Map<Integer, ArrayList<Integer>> map = new HashMap<>();
           for (var q : P.get(cl)) {
               if (!r_ways[q].containsKey(ch)) {
                   continue;
               }
               for (int it : r_ways[q].get(ch)) {
                    int cur_class = Clss[it];
                    if (!map.containsKey(cur_class)) {
                        map.put(cur_class, new ArrayList<>());
                    }
                    map.get(cur_class).add(it);
               }
           }


           for (var I : map.entrySet()) {
               var set_i = P.get(I.getKey());
               int i = I.getKey();
               if (map.get(i).size() >= set_i.size()) {
                   continue;
               }
               P.add(new HashSet<>());
               int j = P.size() - 1;

               var set_j = P.get(j);
               set_j.addAll(map.get(i));
               for (var elem : map.get(i)) {
                   set_i.remove(elem);
               }
               boolean flag = false;
               if (set_j.size() > set_i.size()) {
                   //получение логарифма
                   flag = true;
                   P.set(i, set_j);
                   P.set(j, set_i);

               }
               for (int it : P.get(j)) {
                   //System.out.println(r);
                   Clss[it] = j;
               }
               //System.out.println();
              // System.out.println(P.get(i.getKey()) + "    " + P.get(j));
               for (Character ch_tmp : alph) {
                   Q.push(new Pair_Ch(j, ch_tmp));
               }
//               System.out.println(P);
//               System.out.println(Arrays.toString(Clss));
//               System.out.println();
           }
       }
       System.out.println(P);

        //find 1
        int number_one = 0;
        for (int i = 0; i < P.size(); i++) {
            if (P.get(i).contains(1)) {
                number_one = i;
                break;
            }
        }

       int new_n = P.size();
       HashSet<Integer> bad = new HashSet<>();
       ArrayList<Integer> sigma = new ArrayList<>();
       int num = 2;
       for (int i = 0; i < P.size(); i++) {
           if (i != number_one) {
               boolean flag = false;
               for (var j : P.get(i)) {
                   if (reach[j]) {
                       flag = true;
                       break;
                   }
               }
               if (flag) {
                   sigma.add(num++);
               } else {
                   bad.add(i);
                   new_n--;
               }
           } else {
               boolean flag = false;
               for (var j : P.get(i)) {
                   if (reach[j]) {
                       flag = true;
                       break;
                   }
               }
               if (flag) {
                   sigma.add(1);
               } else {
                   bad.add(i);
                   new_n--;
               }
           }
       }
       ArrayList<Integer> ways_indexis = new ArrayList<>();
       for (int i = 0 ; i < P.size(); i++) {
           if (!bad.contains(i)) {
               ways_indexis.add(i);
           }
       }
//        System.out.println(new_n);
//        System.out.println(sigma);
//        System.out.println(ways_indexis);

        HashMap<Integer, Integer> sigma_w = new HashMap<>();
        for (int i = 0; i < sigma.size(); i++) {
            sigma_w.put(ways_indexis.get(i), sigma.get(i));
        }
        //System.out.println(sigma_w);

       ArrayList<HashMap<Character, Integer>> new_ways = new ArrayList<>();
       for (int i = 0; i < new_n + 1; i++) {
           new_ways.add(new HashMap<>());
       }
       int new_m = 0;
       for (int i = 0; i < P.size(); i++) {
           if (!bad.contains(i)) {
               for (var q : P.get(i)) {
                    if (reach[q]) {
                        var tmp = sigma_w.get(Clss[q]);
                        for (var j : ways[q].entrySet()) {
                            var t = sigma_w.get(Clss[j.getValue()]);
                            if (!new_ways.get(tmp).containsKey(j.getKey()) && t != null) {
                                new_ways.get(tmp).put(j.getKey(), t);
                                new_m++;
                            }
                        }
                        break;
                    }
               }
           }
       }
       // System.out.println(new_ways);
       HashSet<Integer> new_terminals = new HashSet<>();
       for (var it : sigma_w.entrySet()) {
           for (var i : P.get(it.getKey())) {
               if (reach[i] && terminals.contains(i)) {
                   new_terminals.add(it.getValue());
                   break;
               }
           }
       }
//        System.out.println(terminals);
//        System.out.println(new_terminals);
//       int new_m






        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("fastminimization.out"),
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
        out.write(Integer.toString(new_n));
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
