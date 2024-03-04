import levitskiy.hw.task1.Task1;
import levitskiy.hw.task2.Task2;
import levitskiy.hw.task3.Task3;
import levitskiy.hw.task4.Task4;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.fail;


@TestMethodOrder(MethodOrderer.DisplayName.class)
public class TasksTest {
    private final static Random random = new Random(4875043285743285204L);

    @Test
    public void test1Task1() {
        assertIterableEquals(Collections.emptyList(), Task1.convert(Collections.emptyList()));
        assertIterableEquals(List.of(11), Task1.convert(List.of(1)));
        assertIterableEquals(List.of(14, 11), Task1.convert(List.of(2, 1)));
        assertIterableEquals(List.of(11, 11, 11), Task1.convert(List.of(1, 1, 1)));
    }

    @Test
    public void test2Task1() {
        List<Integer> list = List.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14);
        assertIterableEquals(List.of(11, 14, 19, 59, 74, 91, 110, 131, 154, 179), Task1.convert(list));
    }





    @Test
    public void test1Task2() {
        assertEquals(0, Task2.getMapOfDuplicateNumbers(List.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)).size());

        try {
            Task2.getMapOfDuplicateNumbers(List.of(1, 40, 30, 100, 1000));
        } catch (IllegalArgumentException e) {
            return;
        }
        fail("Must be IllegalArgumentException, because 1000 > 100");
    }

    @Test
    public void test2Task2() {
        var map = Task2.getMapOfDuplicateNumbers(
                List.of(1,1,1,1,1,2,3,3,3,1,1,1,2,2,2,2,1,1,11,11,4,5,6,4,7,8)
        );

        assertEquals(5, map.size());
        assertEquals(map.get(1), 10);
        assertEquals(map.get(2), 5);
        assertEquals(map.get(3), 3);
        assertEquals(map.get(4), 2);
        assertEquals(map.get(11), 2);
    }

    @Test
    public void randomTest1Task2() {
        List<Integer> list = new ArrayList<>();
        Map<Integer, Long> map = new HashMap<>();
        for (int i = 0; i < 1000000; i++) {
            int key = random.nextInt(1, 100);
            list.add(key);

            if (map.containsKey(key)) {
                map.put(key, map.get(key) + 1);
            } else {
                map.put(key, 1L);
            }
        }
        var result = Task2.getMapOfDuplicateNumbers(list);
        for (var pair : map.entrySet()) {
            if (pair.getValue() == 1) {
                assertFalse(result.containsKey(pair.getKey()));
            } else {
                assertEquals(pair.getValue(), result.get(pair.getKey()));
            }
        }
    }


    /**
     * Example of work "Warehouse".
     */
    @Test
    public void test1Task3() {
        try (final Task3.Warehouse<Integer> warehouse = new Task3.Warehouse<>()) {
            Supplier<List<Integer>> supplier = () -> {
                try {
                    Thread.sleep(500);
                } catch (InterruptedException e) {
                    //
                }
                var list = IntStream.generate(random::nextInt).limit(random.nextInt(1, 10)).boxed().toList();
                System.out.println("Generate: " + list);
                return list;
            };
            Consumer<Integer> consumer = s -> System.out.println(
                    "Processed: " + s + " in " + Thread.currentThread().getName()
            );

            warehouse.start(supplier, consumer);

            IntStream.range(1, 10).forEach(i -> {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            });
        }
    }





    @Test
    public void test1Task4() {
        try {
            assertEquals("123".repeat(6), new Task4().printAndReturnStr());
        } catch (InterruptedException e) {
            fail();
        }
    }


}
