package com.academy.fintech.pe.grpc.integrate_test.db.payment_schedule_payment;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface TestPaymentSchedulePaymentRepository extends JpaRepository<TestPaymentSchedulePaymentEntity, String> {
    List<TestPaymentSchedulePaymentEntity> findAllByPaymentScheduleId(String paymentScheduleId);
}
