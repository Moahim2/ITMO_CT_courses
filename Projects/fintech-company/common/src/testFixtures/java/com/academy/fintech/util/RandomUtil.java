package com.academy.fintech.util;

import lombok.experimental.UtilityClass;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.Random;
import java.util.stream.Collectors;

@UtilityClass
public final class RandomUtil {
    private static final long RANDOM_SEED = 4875043285743285204L;

    private static final Random RANDOM  = new Random(RANDOM_SEED);

    public static BigDecimal genRandomBigDecimal(double left, double right) {
        return BigDecimal.valueOf(genRandomDoubleValue(left, right));
    }

    public static double genRandomDoubleValue(double left, double right) {
        return RANDOM.nextDouble(left, right);
    }

    public static int genRandomInt(int left, int right) {
        return RANDOM.nextInt(left, right);
    }

    public static String genRandomString() {
        return genRandomString(genRandomInt(1, 40));
    }

    public static String genRandomString(int maxlength) {
        return RANDOM.ints(Character.MIN_CODE_POINT, Character.MAX_CODE_POINT)
                .mapToObj(Character::toString)
                .limit(genRandomInt(1, maxlength))
                .collect(Collectors.joining());
    }

    public static String genRandomDate() {
        while (true) {
            try {
                String year = Integer.toString(genRandomInt(1000, 5000));
                String month = Integer.toString(genRandomInt(1, 12));
                if (month.length() == 1) {
                    month = "0" + month;
                }
                String day = Integer.toString(genRandomInt(1, 31));
                if (day.length() == 1) {
                    day = "0" + day;
                }
                return LocalDate.parse(year + "-" + month + "-" + day).toString();
            } catch (DateTimeParseException ignored) {}
        }
    }
}
