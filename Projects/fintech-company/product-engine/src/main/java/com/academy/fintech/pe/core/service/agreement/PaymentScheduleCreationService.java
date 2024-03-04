package com.academy.fintech.pe.core.service.agreement;

import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementEntity;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule.PaymentScheduleService;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentSchedulePaymentEntity;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentStatus;
import com.academy.fintech.pe.grpc.agreement.v1.dto.DisbursementDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static com.academy.fintech.pe.core.service.agreement.util.FinanceUtil.calculateIPMTs;
import static com.academy.fintech.pe.core.service.agreement.util.FinanceUtil.calculatePMT;
import static com.academy.fintech.pe.core.service.agreement.util.FinanceUtil.calculatePPMT;
import static com.academy.fintech.pe.core.service.agreement.util.FinanceUtil.getRealInterestPerMonth;

@Service
@RequiredArgsConstructor
public class PaymentScheduleCreationService {
    private final PaymentScheduleService paymentScheduleService;

    /**
     * Creating payments and schedule and set {@code disbursementDate} in {@code agreement}.
     *
     * @param agreement agreement.
     * @return id of created schedule.
     */
    @Transactional
    public String createSchedule(AgreementEntity agreement, DisbursementDto disbursementDto) {
        String paymentScheduleId = paymentScheduleService
                .createFirstScheduleVersion(disbursementDto, createAllPayments(agreement, disbursementDto));
        agreement.setDisbursementDate(disbursementDto.disbursementDate());
        return paymentScheduleId;
    }

    /**
     * Calculates all payments.
     * Set nextPaymentDate of {@code agreement} to first payment date of calculated payments.
     *
     * @return full list of payment.
     * @param agreement agreement.
     */
    private List<PaymentSchedulePaymentEntity> createAllPayments(AgreementEntity agreement,
                                                                 DisbursementDto disbursementDto) {
        int loanTerm = agreement.getLoanTermInMonths();
        BigDecimal principalAmount = agreement.getPrincipalAmount();
        BigDecimal realInterestPerMonth = getRealInterestPerMonth(agreement.getInterest());

        List<LocalDate> listDate = calculateDates(loanTerm, disbursementDto.disbursementDate());

        BigDecimal periodPayment = calculatePMT(loanTerm, principalAmount, realInterestPerMonth);
        List<BigDecimal> listInterestPayment = calculateIPMTs(loanTerm, principalAmount, realInterestPerMonth);

        agreement.setNextPaymentDate(listDate.get(0));
        return createListPayments(
                loanTerm,
                listInterestPayment,
                periodPayment,
                listDate);
    }

    /**
     * Calculate impt and ppmt (using pre-made calculations) and save all payments to db.
     * @return list of payment.
     */
    private static List<PaymentSchedulePaymentEntity> createListPayments(int loanTerm,
                                                                         List<BigDecimal> listInterestPayment,
                                                                         BigDecimal periodPayment,
                                                                         List<LocalDate> listDate) {
        return IntStream.range(1, loanTerm + 1).mapToObj(i -> {
            BigDecimal interestPayment = listInterestPayment.get(i - 1);
            BigDecimal principalPayment = calculatePPMT(periodPayment, interestPayment);

            return PaymentSchedulePaymentEntity.builder()
                    .status(PaymentStatus.FUTURE)
                    .paymentDate(listDate.get(i - 1))
                    .periodPayment(periodPayment)
                    .interestPayment(interestPayment)
                    .principalPayment(principalPayment)
                    .periodNumber(i)
                    .build();
        }).toList();
    }

    private static List<LocalDate> calculateDates(int loanTerm, LocalDate curPaymentDate) {
        List<LocalDate> listDate = new ArrayList<>();
        boolean wasLastDayOfMonth = false;
        for (int i = 0; i < loanTerm; i++) {
            if (wasLastDayOfMonth || getCountDaysToMonthEnd(curPaymentDate) == 0) {
                wasLastDayOfMonth = true;
                curPaymentDate = curPaymentDate.plusMonths(1);
                curPaymentDate = curPaymentDate.plusDays(getCountDaysToMonthEnd(curPaymentDate));
            } else {
                curPaymentDate = curPaymentDate.plusMonths(1);
            }
            listDate.add(curPaymentDate);
        }
        return listDate;
    }

    private static int getCountDaysToMonthEnd(LocalDate curPaymentDate) {
        return curPaymentDate.lengthOfMonth() - curPaymentDate.getDayOfMonth();
    }
}
