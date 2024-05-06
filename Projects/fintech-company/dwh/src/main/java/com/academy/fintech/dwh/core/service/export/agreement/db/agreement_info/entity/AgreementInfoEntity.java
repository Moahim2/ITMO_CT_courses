package com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info.entity;

import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "agreement_info")
public class AgreementInfoEntity {
    @EmbeddedId
    private AgreementInfoKey key;

    private String code;
    private String clientId;
    private int loanTerm;
    private BigDecimal principalAmount;
    private BigDecimal interest;
    private BigDecimal originationAmount;
    private String status;
    private LocalDate disbursementDate;
    private LocalDate nextPaymentDate;
    private BigDecimal overdueBalance;
    private BigDecimal mainBalance;
    private Instant createdAt;
}
