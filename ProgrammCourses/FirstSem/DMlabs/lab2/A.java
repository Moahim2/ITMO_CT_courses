import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;
import java.util.PriorityQueue;

public class A {
    static class Node implements Comparable<Node>{
        public BigInteger node;
        public BigInteger count;
        public BigInteger sum;

        @Override
        public int compareTo(Node x) {
            return (node.subtract(x.node)).signum();
        }

        public Node(BigInteger a, BigInteger count, BigInteger sum) {
            node = a;
            this.count = count;
            this.sum = sum;
        }
    }
    
    // static class tree {
    //     public ArrayList<Node> graph = new ArrayList<>();
    //     public tree() {
            
    //     }
    //     public void add(Node a) {
    //         graph.add(a);
    //     }
    // }
    

    static void haff(PriorityQueue<Node> arr, ArrayList<BigInteger> sum) {
        if (arr.size() >= 2) {
            Node x = arr.poll();
            Node y = arr.poll();
            Node newVertex = new Node(x.node.add(y.node), x.count.add(y.count), x.count.add(y.count));
            sum.set(0, sum.get(0).add(newVertex.sum));
            arr.add(newVertex);
            //graph.add(newVertex);
        } else {
            return;
        }
        haff(arr, sum);
    }
    public static void main(String[] args) throws IOException {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        BigInteger[] input = new BigInteger[n];
        for (int i = 0; i < n; i++) {
            input[i] = new BigInteger("1");
            input[i] = new BigInteger(Integer.toString(in.nextInt()));
        }
        //tree graph = new tree();
        //for (int i = 0; i < n; i++) {
          ///  graph.add(new Node(input[i], input[i], input[i], 1, 0));
        //}
        PriorityQueue<Node> arr = new PriorityQueue<>();
        for (int i = 0; i < n; i++) {
            arr.add(new Node(input[i], input[i], input[i]));
        }
        ArrayList<BigInteger> summa = new ArrayList<>(List.of(new BigInteger("0")));
        haff(arr, summa);
        System.out.println(summa.get(0));
    }
}