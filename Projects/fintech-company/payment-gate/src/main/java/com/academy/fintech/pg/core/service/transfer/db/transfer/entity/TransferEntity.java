package com.academy.fintech.pg.core.service.transfer.db.transfer.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "transfer")
public class TransferEntity {
    @Id
    private String id;

    private String agreementId;

    @Enumerated(EnumType.STRING)
    private TransferStatus status;
}
