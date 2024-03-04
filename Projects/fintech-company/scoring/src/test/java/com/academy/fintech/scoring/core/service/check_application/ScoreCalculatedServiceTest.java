package com.academy.fintech.scoring.core.service.check_application;


import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.academy.fintech.scoring.core.service.check_application.ScoreCalculatedService.MAX_COUNT_DAY_OF_WEAK_OVERDUE;
import static com.academy.fintech.scoring.core.service.check_application.ScoreCalculatedService.PAYMENT_SCALE;
import static com.academy.fintech.scoring.core.service.check_application.ScoreCalculatedService.calcScoreOfComparePaymentAndSalary;
import static com.academy.fintech.scoring.core.service.check_application.ScoreCalculatedService.calcScoreOfOverdueDates;
import static com.academy.fintech.util.RandomUtil.genRandomDate;
import static com.academy.fintech.util.RandomUtil.genRandomDoubleValue;
import static com.academy.fintech.util.RandomUtil.genRandomInt;

public class ScoreCalculatedServiceTest {
    @ParameterizedTest
    @CsvSource(value = {
            "10, 100000",
            "200, 700",
            "200, 600",
            "200000, 1000000",
            "0, 0",
            "300, 600",
            "30000, 60000",
            "1000000, 100000"
    })
    public void testCalcScoreOfComparePaymentAndSalary(int pmt, int salary) {
        Assertions.assertEquals(
                pmt * PAYMENT_SCALE.intValue() > salary ? 0 : 1,
                calcScoreOfComparePaymentAndSalary(new BigDecimal(pmt), salary)
        );
    }

    @RepeatedTest(value = 50)
    public void randomTestCalcScoreOfComparePaymentAndSalary() {
        BigDecimal periodPayment = BigDecimal.valueOf(genRandomDoubleValue(0, 1000000000));
        int salary = genRandomInt(0, 1000000000);
        int result = 1;
        if (periodPayment.multiply(PAYMENT_SCALE).compareTo(new BigDecimal(salary)) > 0) {
            result = 0;
        }
        Assertions.assertEquals(
                result,
                calcScoreOfComparePaymentAndSalary(periodPayment, salary));
    }


    @ParameterizedTest
    @MethodSource("argsCountOverdueDatesFactory")
    public void testCalcScoreOfOverdueDates(int[] countOfOverdueDays) {
        LocalDate curDate = LocalDate.now();
        List<LocalDate> overdueDates = Arrays.stream(countOfOverdueDays).mapToObj(curDate::minusDays).toList();
        int result = 1;
        if (Arrays.stream(countOfOverdueDays).anyMatch(i -> i > MAX_COUNT_DAY_OF_WEAK_OVERDUE)) {
            result = -1;
        } else if (countOfOverdueDays.length > 0) {
            result = 0;
        }
        Assertions.assertEquals(result, calcScoreOfOverdueDates(overdueDates));
    }

    @RepeatedTest(50)
    public void testRandomCalcScoreOfOverdueDates() {
        int countDays = genRandomInt(0, 1000);
        List<LocalDate> overdueDates = IntStream
                .range(0, countDays)
                .mapToObj(i -> LocalDate.parse(genRandomDate()))
                .toList();

        LocalDate curDate = LocalDate.now();
        int result;
        if (overdueDates.isEmpty()) {
            result = 1;
        } else {
            boolean flagOfWeakOverdue = overdueDates.stream().allMatch(date ->
                    ChronoUnit.DAYS.between(date, LocalDate.now()) <= MAX_COUNT_DAY_OF_WEAK_OVERDUE
            );
            if (flagOfWeakOverdue) {
                result = 0;
            } else {
                result = -1;
            }
        }
        Assertions.assertEquals(result, calcScoreOfOverdueDates(overdueDates));
    }

    private static Stream<int[]> argsCountOverdueDatesFactory() {
        return Stream.of(
                new int[] {},
                new int[] {1},
                new int[] {7},
                new int[] {1, 2, 3, 4, 5, 6, 7},
                new int[] {8},
                new int[] {100},
                new int[] {1, 2, 3, 4, 8, 7, 5, 10}
        );
    }

}
