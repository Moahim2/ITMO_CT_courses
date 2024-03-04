package com.academy.fintech.pe.core.service.agreement.db.agreement.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
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
@Table(name = "agreement")
public class AgreementEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private String id;
    private String productCode;
    private String clientId;
    @Column(name = "loan_term")
    private int loanTermInMonths;
    private BigDecimal principalAmount;
    private BigDecimal interest;
    private BigDecimal originationAmount;
    @Enumerated(EnumType.STRING)
    private AgreementStatus status;
    private LocalDate disbursementDate;
    private LocalDate nextPaymentDate;
}
