package com.academy.fintech.pe.core.service.payment;

import com.academy.fintech.pe.core.kafka.client.KafkaClientService;
import com.academy.fintech.pe.core.service.agreement.db.agreement.AgreementService;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementEntity;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementStatus;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule.PaymentScheduleService;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.PaymentSchedulePaymentService;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentSchedulePaymentEntity;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentStatus;
import com.academy.fintech.pe.core.service.payment.algorithm.account.v1.BalanceUpdatingStructure;
import com.academy.fintech.pe.core.service.payment.algorithm.account.v1.UpdatePaymentAndAccountAlgorithmService;
import com.academy.fintech.pe.core.service.payment.db.account.AccountService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
@RequiredArgsConstructor
public class PaymentsAndAccountsRegulationService {
    private final AgreementService agreementService;
    private final PaymentScheduleService paymentScheduleService;
    private final PaymentSchedulePaymentService paymentService;
    private final AccountService accountService;
    private final UpdatePaymentAndAccountAlgorithmService updatePaymentAndAccountAlgorithmService;
    private final KafkaClientService kafkaClientService;

    /**
     * Starts recalculation of account's balances and payments for all agreements.
     */
    @Async
    @Scheduled(
            fixedRateString = "${properties.service.payment.make-all-payments.scheduled.fixed-rate}",
            timeUnit = TimeUnit.DAYS
    )
    public void makeAllPayments() {
        log.info("End of day process was starting!");
        agreementService.getAllActiveAgreements()
                .forEach(this::makeAllPaymentsForAgreement);
    }

    /**
     * Starts recalculation of account's balances and payments for this agreement.
     * Save changes to db.
     * Currently, it operates within only two types of accounts ({@code MAIN, OVERDUE})
     * and two types of balances ({@code OVERDUE, FUTURE}) trying to change them into a third type ({@code PAID}).
     * Updates agreement nextPaymentDate and status if required.
     *
     * @param agreement agreement.
     * @see UpdatePaymentAndAccountAlgorithmService#updateAllPayments(BalanceUpdatingStructure structure)
     */
    @Async
    @Transactional
    protected void makeAllPaymentsForAgreement(AgreementEntity agreement) {
        BalanceUpdatingStructure balanceUpdatingStructure = BalanceUpdatingStructure.builder()
                .mainAccount(accountService.getMainAccount(agreement.getId()))
                .overdueAccount(accountService.getOverdueAccount(agreement.getId()))
                .payments(paymentScheduleService.getAllOverdueAndFuturePaymentsInOrder(agreement.getId()))
                .updatedPayments(new ArrayList<>())
                .build();
        updatePaymentAndAccountAlgorithmService.updateAllPayments(balanceUpdatingStructure);
        paymentService.saveAllPayments(balanceUpdatingStructure.updatedPayments());

        accountService.save(balanceUpdatingStructure.mainAccount());
        accountService.save(balanceUpdatingStructure.overdueAccount());

        log.info("MAIN balance of %s is %s"
                .formatted(agreement.getId(), balanceUpdatingStructure.mainAccount().getBalance()));
        log.info("OVERDUE balance of %s is %s"
                .formatted(agreement.getId(), balanceUpdatingStructure.overdueAccount().getBalance()));

        changeNextPaymentDay(agreement, balanceUpdatingStructure);
        closeAgreementIfAllPaymentsArePaid(agreement, balanceUpdatingStructure);

        agreementService.save(agreement);
    }

    private void changeNextPaymentDay(AgreementEntity agreement,
                                             BalanceUpdatingStructure balanceUpdatingStructure) {
        Optional<PaymentSchedulePaymentEntity> nextFuturePaymentOpt = balanceUpdatingStructure
                .payments()
                .stream()
                .filter(payment -> payment.getStatus() == PaymentStatus.FUTURE)
                .min(Comparator.comparing(PaymentSchedulePaymentEntity::getPaymentDate));
        nextFuturePaymentOpt.ifPresent(payment -> agreement.setNextPaymentDate(payment.getPaymentDate()));
    }

    private void closeAgreementIfAllPaymentsArePaid(AgreementEntity agreement,
                                                    BalanceUpdatingStructure balanceUpdatingStructure) {
        if (allPaymentsWasPaid(balanceUpdatingStructure)) {
            log.info("Agreement %s was closed.".formatted(agreement.getId()));
            agreement.setStatus(AgreementStatus.CLOSED);

            kafkaClientService.execute(agreement.getId());
        }
    }

    private boolean allPaymentsWasPaid(BalanceUpdatingStructure balanceUpdatingStructure) {
        return balanceUpdatingStructure
                .payments()
                .stream()
                .allMatch(payment -> payment.getStatus() == PaymentStatus.PAID);
    }
}
