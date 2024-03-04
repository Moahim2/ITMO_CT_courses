package com.academy.fintech.pe.core.service.agreement.db.payment_schedule;

import com.academy.fintech.pe.core.service.agreement.db.payment_schedule.entity.PaymentScheduleEntity;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentSchedulePaymentEntity;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentStatus;
import com.academy.fintech.pe.grpc.agreement.v1.dto.DisbursementDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class PaymentScheduleService {
    private final PaymentScheduleRepository paymentScheduleRepository;

    public String createFirstScheduleVersion(DisbursementDto disbursementDto,
                                             List<PaymentSchedulePaymentEntity> payments) {
        PaymentScheduleEntity paymentSchedule = PaymentScheduleEntity.builder()
                .agreementId(disbursementDto.agreementId())
                .version(1)
                .build();

        payments.forEach(payment -> payment.setPaymentSchedule(paymentSchedule));
        paymentSchedule.setPayments(payments);
        return paymentScheduleRepository.save(paymentSchedule).getId();
    }

    public List<PaymentSchedulePaymentEntity> getAllOverduePayments(String agreementId) {
        return paymentScheduleRepository
                .getPaymentScheduleEntitiesByAgreementId(agreementId)
                .stream()
                .flatMap(schedule -> schedule.getPayments().stream())
                .filter(payment -> payment.getStatus() == PaymentStatus.OVERDUE)
                .toList();
    }
}
