package com.academy.fintech.pe.core.service.payment.algorithm.account.v1;

import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentSchedulePaymentEntity;
import com.academy.fintech.pe.core.service.payment.db.account.entity.AccountEntity;
import lombok.Builder;

import java.util.List;

/**
 * Version of the account and payment regulation structure
 * @param mainAccount account with positive balance (money of client for payments)
 * @param overdueAccount account with negative balance (considered mandatory money of old overdue payments)
 * @param payments all payments for potential changes
 * @param updatedPayments results container
 */
@Builder
public record BalanceUpdatingStructure(
        AccountEntity mainAccount,
        AccountEntity overdueAccount,
        List<PaymentSchedulePaymentEntity> payments,
        List<PaymentSchedulePaymentEntity> updatedPayments
) { }
