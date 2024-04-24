package com.academy.fintech.pe.core.service.payment.db.account.entity;

import jakarta.persistence.Embeddable;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
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
    private String agreementId;
    @Enumerated(EnumType.STRING)
    private AccountType type;
}
