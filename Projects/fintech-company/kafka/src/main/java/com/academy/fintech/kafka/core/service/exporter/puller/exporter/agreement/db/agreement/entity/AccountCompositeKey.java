package com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;

@Embeddable
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class AccountCompositeKey implements Serializable {
    @ManyToOne
    @JoinColumn(name="agreement_id", insertable = false, updatable = false)
    private AgreementEntity agreement;

    @Column(name = "type")
    @Enumerated(EnumType.STRING)
    private AccountType type;
}
