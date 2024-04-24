package com.academy.fintech.pe.core.service.payment.algorithm.account.v1;

import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentSchedulePaymentEntity;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentStatus;
import com.academy.fintech.pe.core.service.payment.db.account.entity.AccountEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * Service for pure algorithms for working with payment and account entities.
 */
@Service
@RequiredArgsConstructor
public class UpdatePaymentAndAccountAlgorithmService {

    /**
     * Regulates balances in accordance with checking and
     * changing the statuses of all payments up to today's date.
     * Works with payments in the order from {@code OVERDUE} to {@code FUTURE}.
     * Increases the {@code OVERDUE} account's balance if
     * there is not enough money on {@code MAIN} account's balance for the payment.
     * Reduces the account's balance if it cancels the payment of the corresponding status.
     *
     * @param balanceUpdatingStructure structure with all parameters and result containers for algorithm.
     * @see #makeAllOldOverduePayments(BalanceUpdatingStructure structure)
     * @see #makeFuturePaymentsOrSetToOverdue(BalanceUpdatingStructure structure, boolean overdueFlag)
     */
    public void updateAllPayments(final BalanceUpdatingStructure balanceUpdatingStructure) {
        boolean overdueFlag = !makeAllOldOverduePayments(balanceUpdatingStructure);
        makeFuturePaymentsOrSetToOverdue(balanceUpdatingStructure, overdueFlag);
    }

    /**
     * Performs all {@code FUTURE} payments up to today's date or sends them in overdue,
     * changing account balances accordingly.
     *
     * @param balanceUpdatingStructure structure with all parameters and result containers for algorithm.
     * @param overdueFlag if we did not have enough money to pay the existing payments.
     */
    private void makeFuturePaymentsOrSetToOverdue(final BalanceUpdatingStructure balanceUpdatingStructure,
                                                  boolean overdueFlag) {
        List<PaymentSchedulePaymentEntity> newPayments = balanceUpdatingStructure
                .payments()
                .stream()
                .filter(payment -> payment.getStatus() == PaymentStatus.FUTURE)
                .filter(payment -> payment.getPaymentDate().isBefore(LocalDate.now()))
                .toList();
        for (PaymentSchedulePaymentEntity payment : newPayments) {
            BigDecimal mainBalance = balanceUpdatingStructure.mainAccount().getBalance();
            if (!overdueFlag && payment.getPeriodPayment().compareTo(mainBalance) <= 0) {
                balanceUpdatingStructure
                        .mainAccount()
                        .setBalance(mainBalance
                                .subtract(payment.getPeriodPayment()));
                payment.setStatus(PaymentStatus.PAID);
            } else {
                overdueFlag = true;
                balanceUpdatingStructure
                        .overdueAccount()
                        .setBalance(balanceUpdatingStructure
                                .overdueAccount()
                                .getBalance()
                                .add(payment.getPeriodPayment()));
                payment.setStatus(PaymentStatus.OVERDUE);
            }
            balanceUpdatingStructure.updatedPayments().add(payment);
        }
    }

    /**
     * Extinguishes (try set status to {@code PAID}) all {@code OVERDUE} payments
     * that can be repaid in the order in which they occur
     * and balances the accounts accordingly.
     *
     * @param balanceUpdatingStructure structure with all parameters and result containers for algorithm.
     * @return true if all overdue payments was paid else false.
     */
    private boolean makeAllOldOverduePayments(final BalanceUpdatingStructure balanceUpdatingStructure) {
        List<PaymentSchedulePaymentEntity> overduePayments = balanceUpdatingStructure
                .payments()
                .stream()
                .filter(payments -> payments.getStatus() == PaymentStatus.OVERDUE)
                .toList();
        AccountEntity overdueAccount = balanceUpdatingStructure.overdueAccount();
        AccountEntity mainAccount = balanceUpdatingStructure.mainAccount();

        for (final PaymentSchedulePaymentEntity payment : overduePayments) {
            BigDecimal paymentValue = payment.getPeriodPayment();

            if (paymentValue.compareTo(balanceUpdatingStructure.mainAccount().getBalance()) <= 0) {
                // we pay off the old overdue payments and adjust the balances
                mainAccount.setBalance(mainAccount.getBalance().subtract(paymentValue));
                overdueAccount.setBalance(overdueAccount.getBalance().subtract(paymentValue));
                payment.setStatus(PaymentStatus.PAID);
                balanceUpdatingStructure.updatedPayments().add(payment);
            } else {
                // there is no way to repay the existing overdue payment
                return false;
            }
        }
        return true;
    }

}
