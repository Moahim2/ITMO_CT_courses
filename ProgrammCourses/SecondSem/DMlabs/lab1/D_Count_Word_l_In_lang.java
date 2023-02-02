import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class D_Count_Word_l_In_lang {
    public static final long c = (10 * 10 * 10) * (10 * 10 * 10) * (10 * 10 * 10) + 7;
    public static void main(String[] args) throws IOException {
        BufferedReader source = new BufferedReader(new InputStreamReader(new FileInputStream("problem4.in")));
        String[] nmkl = source.readLine().split(" ");
        int n = Integer.parseInt(nmkl[0]);
        int m = Integer.parseInt(nmkl[1]);
        int k = Integer.parseInt(nmkl[2]);
        int l = Integer.parseInt(nmkl[3]);

        Set<Integer> terminals = new HashSet<>();
        String[] terminalsInput = source.readLine().split(" ");
        for (int i = 0; i < terminalsInput.length; ++i) {
            terminals.add(Integer.parseInt(terminalsInput[i]));
        }

        HashMap<Integer, Integer>[] ways = new HashMap[n + 1];
        for (int i = 0; i < n + 1; ++i) {
            ways[i] = new HashMap<>();
        }
        for (int i = 0; i < m; ++i) {
            String[] tmp = source.readLine().split(" ");
            if (!ways[Integer.parseInt(tmp[0])].containsKey(Integer.parseInt(tmp[1]))) {
                ways[Integer.parseInt(tmp[0])].put(Integer.parseInt(tmp[1]), 1);
            } else {
                ways[Integer.parseInt(tmp[0])].put(Integer.parseInt(tmp[1]), ways[Integer.parseInt(tmp[0])].
                        get(Integer.parseInt(tmp[1])) + 1);
            }
        }
        source.close();
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
                if (!ways[entry.getKey()].isEmpty()) {
                    for (var q : ways[entry.getKey()].entrySet()) {
                        if (!ans[i].containsKey(q.getKey())) {
                            ans[i].put(q.getKey(), (entry.getValue() * q.getValue()) % c);
                        } else {
                            ans[i].put(q.getKey(), ((entry.getValue()) * q.getValue() + ans[i].get(q.getKey())) % c);
                        }
                    }
                }
            }
        }
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("problem4.out"),
                StandardCharsets.UTF_8));
        if (flag) {
            out.write(Integer.toString(0));
        } else {
            long x = 0;
            for (var entry : ans[ans.length - 1].entrySet()) {
                if (terminals.contains(entry.getKey())) {
                    x = (x + entry.getValue()) % c;
                }
            }
            int u = (int) (x % c);
            out.write(Integer.toString(u));
        }
        out.close();
    }
}
