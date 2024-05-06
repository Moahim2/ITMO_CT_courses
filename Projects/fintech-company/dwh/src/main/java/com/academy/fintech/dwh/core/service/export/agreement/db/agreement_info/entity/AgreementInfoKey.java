package com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info.entity;

import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.time.Instant;
import java.time.LocalDate;

@Embeddable
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class AgreementInfoKey implements Serializable {
    private LocalDate businessDate;
    private String agreementId;
    private Instant updatedAt;
}
