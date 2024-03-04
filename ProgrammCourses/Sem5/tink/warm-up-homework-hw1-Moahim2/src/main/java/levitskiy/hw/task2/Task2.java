package levitskiy.hw.task2;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author Ivan Levitskiy
 */
public class Task2 {

    private final static int MIN = 1;
    private final static int MAX = 100;

    /**
     * The method from the second task of hw.
     * Works amortized for O(n).
     *
     * @param list list
     * @return map when:
     * key is number from list (if this number has got duplicates),
     * value is count of duplicates this number in list.
     *
     * @throws IllegalArgumentException if list contains numbers not in range [{@value #MIN},{@value #MAX}].
     */
    public static Map<Integer, Long> getMapOfDuplicateNumbers(List<Integer> list) {
        if (list.stream().anyMatch(i -> i < MIN || i > MAX)) {
            throw new IllegalArgumentException("List must contains only numbers in range [" + MIN + "," + MAX + "].");
        }
        return list.stream()
                .collect(
                        Collectors.groupingBy(
                                Function.identity(),
                                Collectors.counting()
                        )
                )
                .entrySet().stream()
                .filter(i -> i.getValue() > 1)
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        Map.Entry::getValue
                        )
                );
    }

}
