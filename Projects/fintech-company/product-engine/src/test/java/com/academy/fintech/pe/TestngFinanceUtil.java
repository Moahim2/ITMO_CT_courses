package com.academy.fintech.pe;

import com.academy.fintech.pe.core.service.agreement.util.FinanceUtil;
import org.apache.poi.ss.formula.functions.Finance;
import org.junit.jupiter.api.Assertions;

import java.math.BigDecimal;

public final class TestngFinanceUtil {
    public static final double ASSERTING_SCALE = 0.1;

    public static void comparePmt(BigDecimal r, int nper, BigDecimal pv) {
        BigDecimal rm = getMonthInterest(r);

        double realPmt = Finance.pmt(rm.doubleValue(), nper, pv.doubleValue());
        double appPmt = FinanceUtil.calculatePMT(nper, pv, rm).doubleValue();
        assertWithScale(realPmt, appPmt);
    }

    public static void compareIpmt(BigDecimal r, int per, int nper, BigDecimal pv) {
        BigDecimal rm = getMonthInterest(r);

        double realIpmt = Finance.ipmt(rm.doubleValue(), per, nper, pv.doubleValue());
        double appImpt = FinanceUtil.calculateIPMTs(nper, pv, rm).get(per - 1).doubleValue();
        assertWithScale(realIpmt, appImpt);
    }

    public static void comparePpmt(BigDecimal r, int per, int nper, BigDecimal pv) {
        BigDecimal rm = getMonthInterest(r);

        double realPpmt = Finance.ppmt(rm.doubleValue(), per, nper, pv.doubleValue());
        double appPpmt = FinanceUtil.calculatePPMT(
                FinanceUtil.calculatePMT(nper, pv, rm),
                FinanceUtil.calculateIPMTs(nper, pv, rm).get(per - 1)
        ).doubleValue();
        assertWithScale(realPpmt, appPpmt);
    }

    public static void comparePmt(BigDecimal r, int nper, BigDecimal pv, BigDecimal appPmt) {
        BigDecimal rm = getMonthInterest(r);

        double realPmt = Finance.pmt(rm.doubleValue(), nper, pv.doubleValue());
        assertWithScale(realPmt, appPmt.doubleValue());
    }

    public static void compareIpmt(BigDecimal r, int per, int nper, BigDecimal pv, BigDecimal appIpmt) {
        BigDecimal rm = getMonthInterest(r);

        double realIpmt = Finance.ipmt(rm.doubleValue(), per, nper, pv.doubleValue());
        assertWithScale(realIpmt, appIpmt.doubleValue());
    }

    public static void comparePpmt(BigDecimal r, int per, int nper, BigDecimal pv, BigDecimal appPpmt) {
        BigDecimal rm = getMonthInterest(r);

        double realPpmt = Finance.ppmt(rm.doubleValue(), per, nper, pv.doubleValue());
        assertWithScale(realPpmt, appPpmt.doubleValue());
    }

    public static void assertWithScale(double realVal, double appVal) {
        Assertions.assertEquals(-realVal, appVal, ASSERTING_SCALE);
    }

    public static void assertEqualsBigDecimal(BigDecimal a, BigDecimal b) {
        Assertions.assertEquals(a.doubleValue(), b.doubleValue(), ASSERTING_SCALE);
    }

    private static BigDecimal getMonthInterest(BigDecimal r) {
        return r.divide(
                FinanceUtil.INTEREST_PERIODS_PER_YEAR,
                FinanceUtil.DEFAULT_SCALE,
                FinanceUtil.DEFAULT_ROUNDING_MODE
        );
    }

    private TestngFinanceUtil() {}
}
