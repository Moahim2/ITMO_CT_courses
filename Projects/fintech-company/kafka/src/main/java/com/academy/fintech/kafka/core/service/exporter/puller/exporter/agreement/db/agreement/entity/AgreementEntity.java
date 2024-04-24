package com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.entity;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.util.List;

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

    @Column(name = "product_code")
    private String productCode;
    @Column(name = "client_id")
    private String clientId;
    @Column(name = "loan_term")
    private int loanTermInMonths;
    @Column(name = "principal_amount")
    private BigDecimal principalAmount;
    @Column(name = "interest")
    private BigDecimal interest;
    @Column(name = "origination_amount")
    private BigDecimal originationAmount;
    @Enumerated(EnumType.STRING)
    private AgreementStatus status;
    @Column(name = "disbursement_date")
    private LocalDate disbursementDate;
    @Column(name = "next_payment_date")
    private LocalDate nextPaymentDate;

    @Column(name = "created_at")
    private Instant createdAt;
    @Column(name = "last_business_date")
    private LocalDate lastBusinessDate;

    @OneToMany(mappedBy = "key.agreement",
            fetch = FetchType.EAGER,
            cascade = CascadeType.ALL)
    private List<AccountEntity> accountEntities;
}
