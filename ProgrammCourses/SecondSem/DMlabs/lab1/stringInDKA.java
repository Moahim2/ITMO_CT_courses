import java.io.*;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class stringInDKA {

    public static void main(String[] args) throws IOException {
        BufferedReader source = new BufferedReader(new InputStreamReader(new FileInputStream("problem1.in")));
        String inputString = source.readLine();
        String[] nmk = source.readLine().split(" ");
        int n = Integer.parseInt(nmk[0]);
        int m = Integer.parseInt(nmk[1]);
        int k = Integer.parseInt(nmk[2]);
        Set<Integer> terminals = new HashSet<>();
        String[] terminalsInput = source.readLine().split(" ");
        for (int i = 0; i < terminalsInput.length; ++i) {
            terminals.add(Integer.parseInt(terminalsInput[i]));
        }
        HashMap<Character, Integer>[] ways = new HashMap[n + 1];
        for (int i = 0; i < n + 1; ++i) {
            ways[i] = new HashMap<>();
        }
        for (int i = 0; i < m; ++i) {
            String[] tmp = source.readLine().split(" ");
            ways[Integer.parseInt(tmp[0])].put(tmp[2].charAt(0), Integer.parseInt(tmp[1]));
        }
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("problem1.out")));
        if (k == 0 || n == 0) {
            out.write("Rejects");
            out.close();
            return;
        }
        int current = 1;
        for (int i = 0; i < inputString.length(); ++i) {
            char sym = inputString.charAt(i);
            if (!ways[current].containsKey(sym)) {
                out.write("Rejects");
                out.close();
                return;
            }
            current = ways[current].get(sym);
        }
        if (terminals.contains(current)) {
            out.write("Accepts");
        } else {
            out.write("Rejects");
        }
        out.close();
    }
}
