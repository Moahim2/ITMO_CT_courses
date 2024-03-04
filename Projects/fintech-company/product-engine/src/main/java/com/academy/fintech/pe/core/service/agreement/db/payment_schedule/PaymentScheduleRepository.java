package com.academy.fintech.pe.core.service.agreement.db.payment_schedule;

import com.academy.fintech.pe.core.service.agreement.db.payment_schedule.entity.PaymentScheduleEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface PaymentScheduleRepository extends JpaRepository<PaymentScheduleEntity, String> {
    List<PaymentScheduleEntity> getPaymentScheduleEntitiesByAgreementId(String agreementId);
}
