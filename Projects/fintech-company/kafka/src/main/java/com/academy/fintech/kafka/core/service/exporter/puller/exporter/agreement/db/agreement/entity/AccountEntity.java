package com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.entity;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "account")
public class AccountEntity {
    @EmbeddedId
    private AccountCompositeKey key;
    @Column(name = "balance")
    private BigDecimal balance;
}
