package com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity;

import com.academy.fintech.pe.core.service.agreement.db.payment_schedule.entity.PaymentScheduleEntity;
import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDate;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "payment_schedule_payment")
public class PaymentSchedulePaymentEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "payment_schedule_id")
    @JsonIgnore
    private PaymentScheduleEntity paymentSchedule;

    @Enumerated(EnumType.STRING)
    private PaymentStatus status;
    private LocalDate paymentDate;
    private BigDecimal periodPayment;
    private BigDecimal interestPayment;
    private BigDecimal principalPayment;
    private int periodNumber;
}
