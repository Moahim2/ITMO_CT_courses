package com.academy.fintech.pe.grpc.integrate_test.db.payment_schedule_payment;

import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentStatus;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.math.BigDecimal;
import java.time.LocalDate;

@Entity
@Table(name = "payment_schedule_payment")
public class TestPaymentSchedulePaymentEntity {
    @Id
    public long id;
    @Column(name = "payment_schedule_id")
    private String paymentScheduleId;
    @Enumerated(EnumType.STRING)
    public PaymentStatus status;
    public LocalDate paymentDate;
    public BigDecimal periodPayment;
    public BigDecimal interestPayment;
    public BigDecimal principalPayment;
    public int periodNumber;
    public String getPaymentScheduleId() {
        return paymentScheduleId;
    }
}
