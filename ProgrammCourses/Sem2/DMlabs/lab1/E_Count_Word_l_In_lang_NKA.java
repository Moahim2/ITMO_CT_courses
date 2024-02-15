import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class E_Count_Word_l_In_lang_NKA {

    static class Pair {
        int num;
        HashSet<Integer> map;
        Pair(int q1, HashSet<Integer> q2) {
            this.num = q1;
            this.map = q2;
        }
    }

    public static final long c = (10 * 10 * 10) * (10 * 10 * 10) * (10 * 10 * 10) + 7;
    public static void main(String[] args) throws IOException {
        BufferedReader source = new BufferedReader(new InputStreamReader(new FileInputStream("problem5.in")));
        String[] nmkl = source.readLine().split(" ");
        int n = Integer.parseInt(nmkl[0]);
        int m = Integer.parseInt(nmkl[1]);
        int k = Integer.parseInt(nmkl[2]);
        int l = Integer.parseInt(nmkl[3]);

        Set<Character> alph = new HashSet<>();
        Set<Integer> terminals = new HashSet<>();
        String[] terminalsInput = source.readLine().split(" ");
        for (int i = 0; i < terminalsInput.length; ++i) {
            terminals.add(Integer.parseInt(terminalsInput[i]));
        }

        HashMap<Character, Set<Integer>>[] ways = new HashMap[n + 1];
        for (int i = 0; i < n + 1; ++i) {
            ways[i] = new HashMap<>();
        }
        for (int i = 0; i < m; ++i) {
            String[] tmp = source.readLine().split(" ");
            char ch = tmp[2].charAt(0);
            int a = Integer.parseInt(tmp[0]);
            int b = Integer.parseInt(tmp[1]);
            alph.add(ch);
            if (!ways[a].containsKey(ch)) {
                ways[a].put(ch, new HashSet<>());
            }
            ways[a].get(ch).add(b);
        }
        source.close();


        /////////
        ArrayList<HashMap<Integer, Integer>> new_ways = new ArrayList<>();
        ArrayList<HashMap<Character, Integer>> new_ways_on_ch = new ArrayList<>();
        new_ways_on_ch.add(new HashMap<>());
        new_ways_on_ch.add(new HashMap<>());
        new_ways.add(new HashMap<>());
        new_ways.add(new HashMap<>());
        Set<Integer> terminals2 = new HashSet<>();


        ArrayList<HashSet<Integer>> sets = new ArrayList<>();
        sets.add(new HashSet<>());
        Stack<Pair> stack = new Stack<>();
        HashSet<Integer> q11 = new HashSet<>();
        q11.add(1);
        stack.push(new Pair(1, q11));
        sets.add(q11);
        if (terminals.contains(1)) {
            terminals2.add(1);
        }
        while (!stack.isEmpty()) {
            Pair p = stack.pop();
            HashSet<Integer> q = p.map;
            for (var ch : alph) {
                //System.out.println(ch);
                HashSet<Integer> n_q = new HashSet<>();
                for (var it : q) {
                    for (var it2 : ways[it].entrySet()) {
                        if (it2.getKey() == ch) {
                            n_q.addAll(it2.getValue());
                        }
                    }
                }

               // System.out.println(n_q);
                boolean f = false;
                for (int i = 1; i < sets.size(); ++i) {
                    if (n_q.equals(sets.get(i))) {
                        f = true;

                        HashMap<Character, Integer> copy_ch = new_ways_on_ch.get(p.num);
                        HashMap<Integer, Integer> copy = new_ways.get(p.num);
                        if (!copy_ch.containsKey(ch)) {
                            copy_ch.put(ch, i);
                            if (!copy.containsKey(i)) {
                                copy.put(i, 1);
                            } else {
                                copy.put(i, copy.get(i) + 1);
                            }
                        }
                        break;
                    }
                }

                if (!f) {
                    sets.add(n_q);
                    stack.push(new Pair(sets.size() - 1, n_q));
                   // System.out.println("cefvrvrgvbrgb   " + n_q);
                    new_ways.add(new HashMap<>());
                    new_ways_on_ch.add(new HashMap<>());
                    for (var it3 : n_q) {
                        if (terminals.contains(it3)) {
                            terminals2.add(sets.size() - 1);
                            break;
                        }
                    }

                    HashMap<Character, Integer> copy_ch = new_ways_on_ch.get(p.num);
                    HashMap<Integer, Integer> copy = new_ways.get(p.num);
                    if (!copy_ch.containsKey(ch)) {
                        copy_ch.put(ch, sets.size() - 1);
                        if (!copy.containsKey(sets.size() - 1)) {
                            copy.put(sets.size() - 1, 1);
                        } else {
                            copy.put(sets.size() - 1, copy.get(sets.size() - 1) + 1);
                        }
                    }
                }

            }
        }
//        System.out.println(new_ways);
//        System.out.println(terminals2);
//        System.out.println(sets);
//        System.out.println(new_ways_on_ch);


        boolean flag = false;
        HashMap<Integer, Long>[] ans = new HashMap[l + 1];
        for (int i = 0; i < ans.length; ++i) {
            ans[i] = new HashMap<>();
        }

        ans[0].put(1, 1L);
        for (int i = 1; i < ans.length; ++i) {
            if (ans[i - 1].isEmpty()) {
                flag = true;
                break;
            }
            for (var entry : ans[i - 1].entrySet()) {
                if (!new_ways.get(entry.getKey()).isEmpty()) {
                    for (var q : new_ways.get(entry.getKey()).entrySet()) {
                        if (!ans[i].containsKey(q.getKey())) {
                            ans[i].put(q.getKey(), (entry.getValue() * q.getValue()) % c);
                        } else {
                            ans[i].put(q.getKey(), ((entry.getValue()) * q.getValue() + ans[i].get(q.getKey())) % c);
                        }
                    }
                }
            }
        }
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("problem5.out"),
                StandardCharsets.UTF_8));
        if (flag) {
            out.write(Integer.toString(0));
        } else {
            long x = 0;
            for (var entry : ans[ans.length - 1].entrySet()) {
                if (terminals2.contains(entry.getKey())) {
                    x = (x + entry.getValue()) % c;
                }
            }
            int u = (int) (x % c);
            out.write(Integer.toString(u));
        }
        out.close();
    }
}
