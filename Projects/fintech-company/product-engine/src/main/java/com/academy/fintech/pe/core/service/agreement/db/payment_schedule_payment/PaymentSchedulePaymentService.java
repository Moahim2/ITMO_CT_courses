package com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment;

import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentSchedulePaymentEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class PaymentSchedulePaymentService {
    private final PaymentSchedulePaymentRepository paymentSchedulePaymentRepository;

    public void saveAllPayments(List<PaymentSchedulePaymentEntity> payments) {
        paymentSchedulePaymentRepository.saveAll(payments);
    }
}
