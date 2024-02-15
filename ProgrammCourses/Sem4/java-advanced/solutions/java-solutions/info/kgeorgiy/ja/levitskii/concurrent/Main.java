package info.kgeorgiy.ja.levitskii.concurrent;

import java.util.ArrayList;
import java.util.List;

//PUSHED!!!
public class Main {
    public static void main(String[] args) throws InterruptedException {
        IterativeParallelism parallelism = new IterativeParallelism(new ParallelMapperImpl(10));
        List<Integer> list = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
//            if (i == 5013) {
//                list.add(10000001);
//            }
            list.add(i);
        }
        IterativeParallelism ip = new IterativeParallelism();
        System.out.println(ip.count(3, list, (i) -> i % 2 == 0));
    }
}
