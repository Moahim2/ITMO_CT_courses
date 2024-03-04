package com.academy.fintech.pe.core.service.agreement.util;

import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static com.academy.fintech.pe.TestngFinanceUtil.compareIpmt;
import static com.academy.fintech.pe.TestngFinanceUtil.comparePmt;
import static com.academy.fintech.pe.TestngFinanceUtil.comparePpmt;
import static com.academy.fintech.util.RandomUtil.genRandomBigDecimal;
import static com.academy.fintech.util.RandomUtil.genRandomInt;

/**
 * Unit tests for finance operations.
 */
public class FinanceOperationsTest {

    @ParameterizedTest
    @CsvSource(value = {
            "0.1, 10, 100000",
            "0.213, 200, 992342924294",
            "0.8, 1, 10000.31241"
    })
    public void testPMT(double r, int nper, double pv) {
        comparePmt(new BigDecimal(r), nper, new BigDecimal(pv));
    }

    @ParameterizedTest
    @CsvSource(value = {
            "0.1, 5, 10, 100000",
            "0.213, 8, 200, 992342924294",
            "0.8, 1, 1, 10000.31241"
    })
    public void testIPMT(double r, int per, int nper, double pv) {
        compareIpmt(new BigDecimal(r), per, nper, new BigDecimal(pv));
    }

    @ParameterizedTest
    @CsvSource(value = {
            "0.1, 5, 10, 100000",
            "0.213, 8, 200, 992342924294",
            "0.8, 1, 1, 10000.31241"
    })
    public void testPPMT(double r, int per, int nper, double pv) {
        comparePpmt(new BigDecimal(r), per, nper, new BigDecimal(pv));
    }

    @RepeatedTest(100)
    public void randomTest() {
        BigDecimal r = genRandomBigDecimal(0.1, 0.99);
        BigDecimal amount = genRandomBigDecimal(1000, 100_000_000_000F);
        int per = genRandomInt(1, 100);
        int nper = genRandomInt(0, 100) + per;

        comparePmt(r, nper, amount);
        compareIpmt(r, per, nper, amount);
        comparePpmt(r, per, nper, amount);
    }
}
