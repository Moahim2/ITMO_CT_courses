package prtest.tree;

import base.Selector;
import base.TestCounter;
import prtest.Rule;
import prtest.map.MapChecker;
import prtest.map.State;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.function.Consumer;
import java.util.function.Function;
    
/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class PrologTreeTest {
    private static final Consumer<MapChecker<Void>> FLOOR = test ->
            test.keyChecker(Rule.func("map_floorKey", 2), NavigableMap::floorKey);

    private static final Consumer<MapChecker<Void>> SUB_MAP = test -> test.checker(
            Rule.func("map_subMapSize", 3),
            State::randomKey,
            State::randomKey,
            (k1, k2) -> k1 <= k2 ? map -> map.subMap(k1, k2).size() : map -> 0
    );

    private static final Consumer<MapChecker<Void>> MIN = test ->
            test.checker(Rule.func("map_minKey", 1), get(NavigableMap::firstKey));

    private static final Consumer<MapChecker<Void>> MAX = test ->
            test.checker(Rule.func("map_maxKey", 1), get(NavigableMap::lastKey));


    public static final Selector SELECTOR = new Selector(PrologTreeTest.class, "easy", "hard")
            .variant("base", variant(tests -> {}))
            .variant("Floor", variant(FLOOR))
            .variant("SubMap", variant(SUB_MAP))
            .variant("MinMax", variant(MIN, MAX))

            ;

    private PrologTreeTest() {
    }



    @SafeVarargs
    /* package-private */ static Consumer<TestCounter> variant(final Consumer<MapChecker<Void>>... addTests) {
        return variant(false, addTests);
    }

    @SafeVarargs
    /* package-private */ static Consumer<TestCounter> variant(final boolean alwaysUpdate, final Consumer<MapChecker<Void>>... addTests) {
        return counter -> {
            final boolean hard = counter.mode() == 1;
            PrologTreeTester.test(
                    counter, hard || alwaysUpdate, !hard,
                    tests -> Arrays.stream(addTests).forEachOrdered(adder -> adder.accept(tests))
            );
        };
    }

    private static <K, V> Function<NavigableMap<K, V>, K> get(final Function<NavigableMap<K, V>, K> getter) {
        return map -> map.isEmpty() ? null : getter.apply(map);
    }


    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
