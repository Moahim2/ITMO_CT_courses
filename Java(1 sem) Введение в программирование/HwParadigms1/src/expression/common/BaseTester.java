package expression.common;

import base.Asserts;
import base.BaseChecker;
import base.TestCounter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public abstract class BaseTester extends BaseChecker {
    public final int mode;

    protected BaseTester(final TestCounter counter, final int mode) {
        super(counter);
        this.mode = mode;
        Locale.setDefault(Locale.US);
        Asserts.checkAssert(getClass());
    }

    protected abstract void test();

    @SafeVarargs
    public static <T> List<T> list(final T... items) {
        return new ArrayList<>(Arrays.asList(items));
    }

    public static void addRange(final List<Integer> values, final int d, final int c) {
        for (int i = -d; i <= d; i++) {
            values.add(c + i);
        }
    }
}
