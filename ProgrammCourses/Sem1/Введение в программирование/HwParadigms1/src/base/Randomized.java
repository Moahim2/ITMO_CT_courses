package base;

import java.util.List;
import java.util.Random;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Randomized {
    String ENGLISH = "abcdefghijklmnopqrstuvwxyz";

    static Random initRandom(final long seed, final Class<?> owner) {
        return new Random(seed + owner.getName().hashCode());
    }

    Random getRandom();

    default int randomInt(final int bound) {
        return getRandom().nextInt(bound);
    }

    default int randomInt(final int min, final int max) {
        return getRandom().nextInt(max - min + 1) + min;
    }

    default long randomLong(final long max) {
        return getRandom().nextLong() % max;
    }

    default String randomString(final String chars) {
        return randomChar(chars) + (randomBoolean() ? "" : randomString(chars));
    }

    default boolean randomBoolean() {
        return getRandom().nextBoolean();
    }

    default char randomChar(final String chars) {
        return chars.charAt(getRandom().nextInt(chars.length()));
    }

    @SuppressWarnings("unchecked")
    default <T> T randomItem(final T... items) {
        return items[getRandom().nextInt(items.length)];
    }

    default <T> T randomItem(final List<T> items) {
        return items.get(getRandom().nextInt(items.size()));
    }
}
