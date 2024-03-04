package levitskiy.hw.task1;

import java.util.List;

/**
 * @author Ivan Levitskiy
 */
public class Task1 {

    /**
     * The method from the first task of hw.
     *
     * @param list list
     * @return list where the following transformation happened:
     * i -> i^2 + 10 excluding numbers ending in 5 or 6.
     */
    public static List<Integer> convert(List<Integer> list) {
        return list.stream()
                .map(i -> i * i + 10)
                .filter(i -> i % 10 != 5 && i % 10 != 6)
                .toList();
    }

}
