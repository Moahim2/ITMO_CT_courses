package com.academy.fintech.scoring.core.service.check_application;

import com.academy.fintech.scoring.grpc.scoring.v1.dto.CheckApplicationDto;
import com.academy.fintech.scoring.public_interface.scoring_information.dto.ScoringInformationDto;
import lombok.experimental.UtilityClass;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.List;

@UtilityClass
public class ScoreCalculatedService {
    static final BigDecimal PAYMENT_SCALE = new BigDecimal(3);
    static final int MAX_COUNT_DAY_OF_WEAK_OVERDUE = 7;

    /**
     * Calculate score on rules:
     * <ul>
     *     <li>
     *         1) The amount of the regular loan payment should not exceed one third of the client's salary.
     *            If the data satisfies the condition, then it is 1 scoring point, otherwise 0.
     *     </li>
     *     <li>
     *         2) Checking existing loans: if the client has an existing loan and
     *         he has a delay of more than 7 days in it, then -1 point,
     *         if the delay is up to 7 days, then 0 points,
     *         if there is a loan without delay or no loan, then 1 point.
     *     </li>
     * </ul>
     *
     * @param scoringInformation information about overdue and payment amount.
     * @param checkApplicationDto information about client.
     * @return score, which sum result of all rules.
     */
    public static int calcFullScore(ScoringInformationDto scoringInformation, CheckApplicationDto checkApplicationDto) {
        int salaryScore = calcScoreOfComparePaymentAndSalary(
                scoringInformation.periodPayment(),
                checkApplicationDto.salary()
        );
        int overdueScore = calcScoreOfOverdueDates(scoringInformation.overdueDates());
        return salaryScore + overdueScore;
    }

    /**
     * Calculates score for rule 1.
     * @see #calcFullScore(ScoringInformationDto, CheckApplicationDto)
     */
    static int calcScoreOfComparePaymentAndSalary(BigDecimal periodPayment, int salary) {
        int compareFlag = periodPayment
                .multiply(PAYMENT_SCALE)
                .compareTo(BigDecimal.valueOf(salary));
        if (compareFlag > 0) {
            return 0;
        } else {
            return 1;
        }
    }

    /**
     * Calculates score for rule 2.
     * @see #calcFullScore(ScoringInformationDto, CheckApplicationDto)
     */
    static int calcScoreOfOverdueDates(List<LocalDate> overdueDates) {
        if (overdueDates.isEmpty()) {
            return 1;
        }

        boolean flagOfWeakOverdue = overdueDates.stream().allMatch(date ->
                getDiffInDaysBetweenDateAndCurMoment(date) <= MAX_COUNT_DAY_OF_WEAK_OVERDUE
        );
        if (flagOfWeakOverdue) {
            return 0;
        } else {
            return -1;
        }
    }

    private static long getDiffInDaysBetweenDateAndCurMoment(LocalDate date) {
        return ChronoUnit.DAYS.between(date, LocalDate.now());
    }
}
