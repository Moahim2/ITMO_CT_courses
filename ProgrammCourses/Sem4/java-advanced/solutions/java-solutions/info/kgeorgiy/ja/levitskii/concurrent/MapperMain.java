package info.kgeorgiy.ja.levitskii.concurrent;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.function.Function;

public class MapperMain {
    public static void main(String[] args) throws InterruptedException {
        ParallelMapperImpl mapper = new ParallelMapperImpl(10);
        IterativeParallelism iterativeParallelism1 = new IterativeParallelism(mapper);
        IterativeParallelism iterativeParallelism2 = new IterativeParallelism(mapper);
        Function<Integer, Boolean> f1 = i -> i % 2 == 0;
        ArrayList<Integer> list1 = new ArrayList<>();
        for (int i = 0; i < 500000; i++) {
            list1.add(i);
        }

        Function<Integer, Boolean> f2 = i -> i % 5 == 0;
        ArrayList<Integer> list2 = new ArrayList<>();
        for (int i = 0; i < 500000; i++) {
            if (i == 200000) {
                list2.add(123456780);
            } else {
                list2.add(i);
            }
        }

        Thread t1 = new Thread(() -> {
            try {
                System.out.println(mapper.map(f1, list1).stream().filter(i -> i).count() + " : t1");
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        });

        Thread t2 = new Thread(() -> {
            try {
                System.out.println(mapper.map(f2, list2).stream().filter(i -> i).count() + " : t2");
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        });

        Thread t3 = new Thread(() -> {
            try {
                System.out.println(iterativeParallelism1.maximum(30, list1, Comparator.naturalOrder()) + " : t3");
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        });

        Thread t4 = new Thread(() -> {
            try {
                System.out.println(iterativeParallelism2.maximum(500, list2, Comparator.naturalOrder()) + " : t4");
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        });

        t1.start();
        t2.start();
        t3.start();
        t4.start();
        t1.join();
        t2.join();
        t3.join();
        t4.join();
        mapper.close();
    }
}
