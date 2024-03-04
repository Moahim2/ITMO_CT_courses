package com.academy.fintech.pe.core.service.agreement.util;

import lombok.experimental.UtilityClass;

import java.math.BigDecimal;

@UtilityClass
public class LoanUtil {
    public static final int DEFAULT_LOAN_TERM = 12;
    public static final BigDecimal DEFAULT_ORIGINATION_AMOUNT = new BigDecimal(100);
    public static final BigDecimal DEFAULT_INTEREST_PER_YEAR = BigDecimal.TEN;
    public static final BigDecimal DEFAULT_INTEREST_PER_MONTH =
            FinanceUtil.getRealInterestPerMonth(DEFAULT_INTEREST_PER_YEAR);

    public static BigDecimal calculateDefaultPeriodPayment(int disbursementAmount) {
        return FinanceUtil.calculatePMT(
                LoanUtil.DEFAULT_LOAN_TERM,
                BigDecimal.valueOf(disbursementAmount).add(LoanUtil.DEFAULT_ORIGINATION_AMOUNT),
                LoanUtil.DEFAULT_INTEREST_PER_MONTH
        );
    }

}
