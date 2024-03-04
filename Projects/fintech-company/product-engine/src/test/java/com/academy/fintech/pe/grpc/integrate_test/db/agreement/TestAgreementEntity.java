package com.academy.fintech.pe.grpc.integrate_test.db.agreement;

import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementStatus;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.math.BigDecimal;
import java.time.LocalDate;

@Entity
@Table(name = "agreement")
public class TestAgreementEntity {
    @Id
    public String id;
    public String productCode;
    public String clientId;
    @Column(name = "loan_term")
    public int loanTermInMonths;
    public BigDecimal principalAmount;
    public BigDecimal interest;
    public BigDecimal originationAmount;
    @Enumerated(EnumType.STRING)
    public AgreementStatus status;
    public LocalDate disbursementDate;
    public LocalDate nextPaymentDate;
}
