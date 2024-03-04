package com.academy.fintech.pe.core.service.agreement.util;

import lombok.experimental.UtilityClass;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collections;
import java.util.List;
import java.util.stream.IntStream;

@UtilityClass
public final class FinanceUtil {
    public static final BigDecimal INTEREST_PERIODS_PER_YEAR = BigDecimal.valueOf(12);
    public static final int DEFAULT_SCALE = 5;
    public static final RoundingMode DEFAULT_ROUNDING_MODE = RoundingMode.CEILING;

    public static BigDecimal getRealInterestPerMonth(BigDecimal interestPerYear) {
        return getDoubleValueOfInterest(interestPerYear)
                .divide(INTEREST_PERIODS_PER_YEAR, DEFAULT_SCALE, DEFAULT_ROUNDING_MODE);
    }

    public static BigDecimal getDoubleValueOfInterest(BigDecimal interestPerYear) {
        return interestPerYear
                .divide(BigDecimal.valueOf(100), DEFAULT_SCALE, DEFAULT_ROUNDING_MODE);
    }

    /**
     *
     * @param loanTerm loan of term (in months).
     * @param principalAmount all amount.
     * @param interest interest of loan (per month, in double value)
     * @return pmt.
     */
    public static BigDecimal calculatePMT(int loanTerm, BigDecimal principalAmount, BigDecimal interest) {
        if (interest.doubleValue() == 0) {
            return principalAmount.divide(BigDecimal.valueOf(loanTerm), DEFAULT_SCALE, DEFAULT_ROUNDING_MODE);
        }

        BigDecimal rtn = BigDecimal.ONE.add(interest).pow(loanTerm);
        return interest
                .multiply(principalAmount)
                .multiply(rtn)
                .divide(rtn.subtract(BigDecimal.ONE), DEFAULT_SCALE, DEFAULT_ROUNDING_MODE);
    }

    /**
     *
     * @param loanTerm loan of term (in months).
     * @param principalAmount all amount.
     * @param interest interest of loan (per month, in double value)
     * @return list of impt for all periods.
     */
    public static List<BigDecimal> calculateIPMTs(int loanTerm, BigDecimal principalAmount, BigDecimal interest) {
        if (interest.doubleValue() == 0) {
            return Collections.nCopies(loanTerm, BigDecimal.ZERO);
        }

        BigDecimal rt = interest.add(BigDecimal.ONE);
        BigDecimal rightTerm = interest
                .add(BigDecimal.ONE)
                .multiply(rt
                        .pow(loanTerm)
                        .subtract(BigDecimal.ONE));
        BigDecimal pi = principalAmount.multiply(interest);
        BigDecimal rtn = rt.pow(loanTerm + 1);

        return IntStream.range(1, loanTerm + 1).mapToObj(numberOfPayment -> pi
                .multiply(rtn.subtract(rt.pow(numberOfPayment)))
                .divide(rightTerm, DEFAULT_SCALE, DEFAULT_ROUNDING_MODE)
        ).toList();
    }

    /**
     *
     * @param pmt pmt (for a reasonable result, it must be agreed with {@code ipmt})
     * @param ipmt ipmt (for a reasonable result, it must be agreed with {@code pmt})
     * @return ppmt
     */
    public static BigDecimal calculatePPMT(BigDecimal pmt, BigDecimal ipmt) {
        return pmt.subtract(ipmt);
    }

}
